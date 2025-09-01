library("readxl")
library("tidyverse")
source("functions/helper_functions.R")
source("functions/check_cleaning.R")

# Where to read from the Excel spreadsheets ===================================

# List of positions, where the data is located in the Excel file - different
# for each park. Labels winter and summer refer to the data version here, rather
# than the season, when the sample was taken.
data_positions <- list(
  Bay_Wald = c(winter = "A - BayWald!A1:AH14", summer = "A - BayWald!A1:AR26"),
  Hainich = c(winter = "C - Hainich!A1:AP51", summer = "C - Hainich!A1:AI6"),
  Hunsrueck = c(winter = NA, summer = "D - Hunsrück Hochwald!A1:AV54"),
  Saechs_Schw = c(
    winter = "E - SächsSchweiz!A1:AH12",
    summer = "E - SächsSchweiz!A1:AP58"
  ),
  Jasmund = c(winter = "G - Jasmund!A1:AP51", summer = NA),
  Kellerwald = c(winter = NA, summer = "L - Kellerwald Edersee!A1:AS51"),
  Eifel = c(winter = "Z - Eiffel!A1:AL51", summer = "Z - Eiffel!A1:AZ31"),
  Vorpomm = c(winter = "F - VorpommBoddenlandschaft!A1:AK53", summer = NA)
)

# Load the data from the Excel sheets ==========================================

data_versions <- c(
  winter = "data/20250120_Auswertung_Winter23_24.xlsx",
  summer = "data/20250627_Auswertung_Sommer24.xlsx"
)
sheets <- expand.grid(
  Park = names(data_positions),
  Data_version = c("winter", "summer")
)
raw_data_list <- list()
for (k in 1:nrow(sheets)) {
  park_to_load <- sheets[k, "Park"]
  data_version_to_load <- sheets[k, "Data_version"]
  if (!is.na(data_positions[[park_to_load]][data_version_to_load])) {
    dat <- read_excel(
      data_versions[data_version_to_load],
      range = data_positions[[park_to_load]][data_version_to_load],
      col_types = "text"
    ) |>
      rename(
        # Localize to English
        "Date_of_sample_collection" = "Datum Erlegung",
        "Species" = "Tierart",
        "Sex" = "Geschlecht",
        "Age" = "Alter"
      ) |>
      mutate(
        Park = park_to_load
      ) |>
      rename_columns()
    # Rename the 'Probennummer - ' column, which should come first
    colnames(dat)[1] <- "Sample_number"
    raw_data_list <- c(raw_data_list, list(dat))
  }
}

raw_data <- bind_rows(raw_data_list) |>
  mutate(
    # Format the dates and relabel the factors
    Date_of_sample_collection = as.Date(
      as.numeric(`Date_of_sample_collection`),
      origin = "1899-12-30"  # Excel starts from this date
    ),
    Sex = relabel_sex(Sex),
    Species = relabel_species(Species),
    Age = relabel_age(Age)
  ) |>
  rename(
    "Age_as_indicated_by_Parks" = "Alter (vom Park angegeben)"
  )

# Correct errors and peculiarities =============================================

clean_data <- raw_data |>
  # filter out duplicate rows
  filter(!(Sample_number %in% c("F19b", "F20b"))) |>
  # Drop the only C. capreolus
  filter(Sample_number != "G21") |>
  # Filter out observations, where we have no date. This should be only A60.
  filter(Sample_number != "A60") |>
  dplyr::select(
    # Filter out  unnecessary information
    -c(
      "Interne Nummer",
      "Bemerkung",
      "x",
      "y",
      "Revier Lang",
      "Gemeinde",
      "Jagdbezirk",
      "Gewicht [kg]",
      "Anmerkung zum Gewicht"
    )
  )

# Fix mistyped dates
clean_data[clean_data$Sample_number == "F15", "Date_of_sample_collection"] <-
  as.Date("2023-11-30")
clean_data[clean_data$Sample_number == "F37", "Date_of_sample_collection"] <-
  as.Date("2023-11-24")
clean_data[clean_data$Sample_number == "Z73", "Date_of_sample_collection"] <-
  as.Date("2024-08-03")

# Fix the incorrect age category
clean_data[clean_data$Sample_number == "F35", "Age"] <- "adult"

# Fix confirmed incorrect values
clean_data[clean_data$Sample_number == "G02", "OC (Octocrylene)"] <- "<10"
clean_data[clean_data$Sample_number == "D71", "Clopidogrel"] <- "<1"

# Load the list of measured chemicals ==========================================

# Load the list of the chemicals. We load from the second data batch, as the
# first one is a subset
chem_categories <- read_excel(
  "data/20250627_Auswertung_Sommer24.xlsx",
  range = "Stoffübersicht!A1:E62",
  col_types = "text"
) %>%
  rename(
    # Localize to English
    Chemical = Name,
    primary_category = "primäre Kategorie",
    Quantification_threshold = "LOQ (µg/kg)"
  ) %>%
  dplyr::select(
    Chemical,
    primary_category,
    Quantification_threshold
  ) %>%
  mutate(
    Quantification_threshold = as.numeric(Quantification_threshold),
    primary_category = factor(
      primary_category,
      labels = c(
        "Anthropogenic pollution",
        "API",
        "Industrial chemical",
        "PAH",
        "PCP",
        "Pesticide",
        "POP",
        "Plasticizer"
      )
    ),
    # Fix misspelled names of the chemicals
    Chemical = case_when(
      Chemical == "Uvinul® A plus" ~ "Uvinul A Plus",
      Chemical == "1,3-Dinitrobenzen" ~ "1,3-Dinitrobenzene",
      Chemical == "Diphenylamin" ~ "Diphenylamine",
      Chemical == "2,4,4'-Trichlorobiphenyl (PCB #52)" ~
        "2,2',5,5'-Tetrachlorobiphenyl (PCB #52)",
      .default = Chemical
    )
  )

# Do some checks of the cleaned data ===========================================
# If something's wrong, these checks display a warning. Otherwise, everything is
# OK.
check_age_categories(clean_data)
# Drop the age entries indicated by the parks, as it's not needed anymore
clean_data <- clean_data |> select(-"Age_as_indicated_by_Parks")
check_complete_entries(clean_data)
check_measured_vs_overview(
  clean_data,
  chem_categories$Chemical
)

# Save the cleaned data and classification of the chemicals ====================
# Save as .csv
write_csv(clean_data, file = "data/clean_data.csv")
write_csv(chem_categories, file = "data/chemical_categories.csv")

# Save as Excel
wb <- save_data_as_xls(clean_data)
openxlsx::saveWorkbook(
  wb,
  "data/clean_data.xlsx",
  overwrite = TRUE
)
