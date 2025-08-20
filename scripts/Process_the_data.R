library("tidyverse")
source("functions/helper_functions.R")
source("functions/process_data.R")

# Process the main deer data ===================================================

chem_categories <- read_csv("data/chemical_categories.csv")
dat_deer <- read_csv("data/clean_data.csv")
df_detected_by_category <- process_data(dat_deer, chem_categories)
write_csv(df_detected_by_category, file = "data/data_by_pollutant_category.csv")

# Process the roe deer data ====================================================

dat_roe <- read_csv("data/clean_roe_deer_data.csv") |>
  mutate(Park = "Non-Park")

# Find a subset of the measured chemicals
chem_categories_deer <- chem_categories |>
  filter(Chemical %in% colnames(dat_roe))

# Chemicals discovered only for the roe deer samples. They were not discovered
# in any samples in the main deer data, but they were indeed looked for.
chem_categories_roe <- tibble(
  Chemical = c("DDT (p,p' and o,p')", "gamma-HCH", "Permethrin"),
  primary_category = c("POP", "POP", "Pesticide"),
  Quantification_threshold = c(2, 1, 4)
)

# The subset of chemicals measured for both datasets
chem_categories_subset <- rbind(chem_categories_deer, chem_categories_roe)

# Combine both datasets. bind_rows() introduces NA values for columns, that
# does not appear in both data frames. This is exactly what we want, because the
# NA values represent substances, that were not discovered in a sample.
dat <- bind_rows(dat_deer, dat_roe) |>
  select(
    c(
      "Park",
      "Sample_number",
      "Date_of_sample_collection",
      chem_categories_subset$Chemical
    )
  )

# Process and write
df_detected_by_category <- process_data(dat, chem_categories_subset)
write_csv(
  df_detected_by_category,
  file = "data/data_non_park_comparison_by_pollutant_category.csv"
)
