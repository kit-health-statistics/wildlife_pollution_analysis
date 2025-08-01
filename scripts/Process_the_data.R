library("tidyverse")
source("functions/helper_functions.R")

# Read the cleaned data ========================================================

chem_categories <- read_csv("data/chemical_categories.csv")
dat <- read_csv("data/clean_data.csv") %>%
  # Convert the measurements to character to avoid problems when pivoting
  mutate_at(vars(-Age, -Species, -Sex, -Season), as.character)

# Reshape the data to the long format ==========================================

dat_long <- dat %>%
  pivot_longer(
    -c(
      Park,
      Sample_number,
      Species,
      Sex,
      Age,
      Date_of_sample_collection,
      Season
    ),
    names_to = "Chemical",
    values_to = "Value"
  ) %>%
  mutate(
    # Throws warnings, because we use as.numeric() on NA values, but it is OK
    Detected = case_when(
      # When a cell is empty, the chemical was not detected
      is.na(Value) ~ "Not detected",
      # Not quantified values contain the "<" character
      grepl("<", Value) ~ "Detected",
      # Quantified values are values that can be converted to a numeric
      !is.na(as.numeric(Value)) ~ "Quantified"
    ),
    # Throws warnings, because we use as.numeric() on NA values, but it is OK
    Value = ifelse(Detected == "Quantified", as.numeric(Value), NA)
  )

# Assign the category to the chemicals
dat_long <- left_join(dat_long, chem_categories, by = "Chemical")

# Handle the detection of chemicals by category
df_detected_by_category <- dat_long %>%
  group_by(
    primary_category,
    Park,
    Sample_number,
    Sex,
    Season,
    Species,
    Age,
    Date_of_sample_collection
  ) %>%
  summarise(
    Value_sum_by_category = sum(Value, na.rm = TRUE),  # Redundant column
    Value_sum_by_category_left_censored = list(
      summarise_censoring(Detected, Value, Detection_threshold)
    ),
    Detected_by_category = summarise_detection(Detected)
  ) %>%
  ungroup() %>%
  unnest_wider(Value_sum_by_category_left_censored)

# Save the processed data ======================================================

write_csv(df_detected_by_category, file = "data/data_by_pollutant_category.csv")
