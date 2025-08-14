library("readxl")
library("tidyverse")

# Where to read from the Excel spreadsheets ===================================

# List of positions, where the data is located in the Excel file. This ordering
# different to the deer data. The data is grouped by the chemical category
# rather than the location. There are roe deer and boar samples. The roe deer
# samples are located on rows 14--71. Column headers are in rows 12--14.
# There is sometimes another column in the Excel sheet, which summarizes whole
# groups of chemicals. We do not include these in our range to load, since we
# summarize the concentrations ourselves.
data_positions <- list(
  PCBs = "PCBs!B12:S71",
  OC_Insecticides = "OC Insecticides!B12:T71",
  Other_Insecticides = "Other Insecticides!B12:M71",
  Fungicides = "Fungicides!B12:P71"
)

# Load the data from the Excel sheets ==========================================

raw_data_list <- vector("list", 4)
for (k in seq_along(data_positions)) {
  # Load the excel files
  raw_data_list[[k]] <- read_excel(
    "data/Daten_Wildtiere_Sachsen_ohne_PFAS.xlsx",
    range = data_positions[[k]],
    col_types = "text"
  )

  # Fix the column names, which are split into 2 rows of the excel sheet.
  # The first row is read as column names, the second row as the first row of
  # the data.
  colnames_fixed <- ifelse(
    is.na(raw_data_list[[k]][1, ]),
    colnames(raw_data_list[[k]]),
    raw_data_list[[k]][1, ]
  ) |>
    unlist()
  # Remove excessive spacing from the column names
  colnames(raw_data_list[[k]]) <- str_squish(colnames_fixed)
  # Remove the first row with the column names artefacts
  raw_data_list[[k]] <- raw_data_list[[k]][-1, ]

  # Remove columns that we don't need. We should be left with sample numbers,
  # date information, species and the chemical measurements. Compared to the
  # original deer data, we do not have the information about sex and age of the
  # animals.
  raw_data_list[[k]] <- raw_data_list[[k]] |>
    select(
      -c(
        "Laufende Nummer",
        "Proben-Nr. (Deckel)",
        "postal code",
        "town / district",
        "state",
        "country"
      )
    )
}

# Select and format the columns
raw_data <- purrr::reduce(
  raw_data_list,
  full_join,
  by = c("No.", "Species", "day", "month", "year")
) |>
  rename("Sample_number" = "No.") |>
  mutate(
    # Format the date
    Date_of_sample_collection = make_date(year, month, day),
    # Rename the species (only a cosmetic change)
    Species = "C. capreolus",
    # Add the identifier 'CC' to the sample number to label the data batch
    Sample_number = paste0("CC", Sample_number)
  ) |>
  # Remove the redundant date columns
  select(-c("day", "month", "year")) |>
  # Remove the columns combining several chemicals
  select(
    -c(
      "PCB 28, PCB 52, PCB 101 [µg kg-1]",
      "DDE, DDT [µg kg-1]",
      "HCH [µg kg-1]"
    )
  ) |>
  # Rename the columns so that the chemical names agree to those from the main
  # dataset.
  rename(
    "2,4,4'-Trichlorobiphenyl (PCB #28)" = "PCB 28 [µg kg-1]",
    "2,2',5,5'-Tetrachlorobiphenyl (PCB #52)" = "PCB 52 [µg kg-1]",
    "2,2',4,5,5'-Pentachlorobiphenyl (PCB #101)" = "PCB 101 [µg kg-1]",
    "2,2',3,4,4',5'-Hexachlorobiphenyl (PCB #138)" = "PCB 138 [µg kg-1]",
    "2,2',4,4',5,5'-Hexachlorobiphenyl (PCB #153)" = "PCB 153 [µg kg-1]",
    "2,2',3,4,4',5,5'-Heptachlorobiphenyl (PCB #180)" = "PCB 180 [µg kg-1]",
    "DDE-p,p'" = "DDE (p,p') [µg kg-1]",
    "Dieldrin" = "Dieldrin [µg kg-1]",
    "BHC-beta" = "beta-HCH [µg kg-1]",
    "Epoxiconazole" = "Epoxiconazole [µg kg-1]",
    "oxy-Chlordane" = "Oxychlordane [µg kg-1]",
    "Propiconazole" = "Propiconazole* [µg kg-1]",
    "Tebuconazole" = "Tebuconazole [µg kg-1]",
    "Tetraconazole" = "Tetraconazole [µg kg-1]",
    # The following chemicals are not in the overview of the original data.
    # It means that they were measured there too, but not discovered in a single
    # sample.
    "gamma-HCH" = "gamma-HCH (Lindane) [µg kg-1]",
    "Permethrin" = "Permethrin [µg kg-1]",
    "DDT (p,p' and o,p')" = "DDT (p,p' and o,p') [µg kg-1]",
  )

# Replace the 0 values by NAs to make the dataset compatible with the original
# data.
clean_data <- raw_data |>
  mutate(
    across(
      -c("Sample_number", "Species", "Date_of_sample_collection"),
      function(x) {
        ifelse(x == 0, NA, x)
      }
    )
  )

# Save the cleaned data
write_csv(clean_data, file = "data/clean_roe_deer_data.csv")
