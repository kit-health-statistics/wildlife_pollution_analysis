# Check that the age category was matches the data indicated by parks, where
# this extra column is available.
check_age_categories <- function(data) {
  parks_with_extra_age_column <- list(
    numeric = c("Jasmund", "Vorpomm"),  # Here is the age as a numeric value
    character = "Saechs_Schw"  # Here is the age as a character string
  )
  age_numeric <- c(TRUE, FALSE)

  for (k in 1:2) {
    age_data <- data |>
      filter(Park %in% parks_with_extra_age_column[[k]]) |>
      dplyr::select(c("Sample_number", "Age", "Age_as_indicated_by_Parks"))
    age_category_match <- with(
      age_data,
      Age ==
        age_category_from_extra_column(
          Age_as_indicated_by_Parks,
          age_numeric[k]
        )
    )
    age_mismatch_sample <-
      age_data[!age_category_match, "Sample_number"] |>
      unlist() |>
      unname()
    if (!all(age_category_match)) {
      warning(
        paste0(
          "Age category does not match the value indicated by the park. Please check sample ",  # nolint
          age_mismatch_sample
        )
      )
    }
  }
}

# Check that we have complete covariates. For the date of sample collection we
# know that it's missing for sample A60
check_complete_entries <- function(data) {
  entries_incomplete <- data |>
    dplyr::select(
      c("Sample_number", "Species", "Sex", "Age", "Park", "Season")
    ) |>
    anyNA()

  if (entries_incomplete) {
    warning("Some entries of the cleaned data are incomplete.")
  }
}

# Check that everything we measured is in the overview and vice versa
# 'chemicals' is a character vector with the chemical names from the overview
check_measured_vs_overview <- function(data, chemicals) {
  measured_chemicals <- data |>
    dplyr::select(
      -c(
        "Sample_number",
        "Species",
        "Sex",
        "Age",
        "Park",
        "Season",
        "Date_of_sample_collection"
      )
    ) |>
    colnames()

  if (!all(measured_chemicals %in% chemicals)) {
    warning(
      "A chemical was measured that was not in the overview. Possible misspelling."  # nolint
    )
  }
  if (!all(chemicals %in% measured_chemicals)) {
    warning(
      "A chemical that is in the overview was not measured anywhere. Shall we include it as a non-detect everywhere?"  # nolint
    )
  }
}
