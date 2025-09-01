#' Process concentration measurements into values by category
#'
#' @description This function aggregates the concentration values of individual
#'    substances into sums by chemical category. The act of defining the
#'    aggregated values takes place in the \code{summarise_censoring} function.
#'    In addition, this function drops the chemicals, for which the values are
#'    too uninformative.
#' @param dat A data frame containing concentration measurements with metadata
#'    columns `Park`, `Sample_number`, `Species`, `Sex`, `Age` and
#'    `Date_of_sample_collection`
#' @param chem_categories A data frame with columns `Chemical`,
#'    `primary_category`, and `Quantification_threshold`
#' @return A data frame with aggregated measurements by category including
#'    detection status and interval bounds for best/worst-case
#'    scenarios
#' @details All chemicals are dropped from the analysis, for which:
#'    \begin{itemize}
#'      \item no "Quantified" value was found and at the same time
#'      \item the proportion of the "Detected" values is less than 5 %
#'    \end{itemize}
#'
#'    Overall detection status:
#'    \begin{itemize}
#'      \item "Quantified": At least one sample in a category contains a
#'      quantified value.
#'      \item "Detected": No sample contains a quantified value and at least one
#'      sample in a category contains a detected value under the quantification
#'      limit.
#'      \item "Not detected": All samples are non-detects.
#'    \end{itemize}
#'    Definition of the best/worst-cases:
#'    \begin{itemize}
#'      \item For all samples containing only non-detects or non-quantifiable
#'      values, the best case is a (near) 0 value and the worst case is the sum
#'      of the detection thresholds.
#'      \item When at least value is quantified, the non-detects and
#'      non-quantifiable ones are resolved as above and then the sum of all
#'      quantifiable values is added to the both best and worst case values.
#'    \end{itemize}
process_data <- function(dat, chem_categories) {
  # Reshape the data to a long format
  dat_long <- dat |>
    # Convert the measurements to character to avoid problems when pivoting
    mutate(across(-tidyselect::any_of("Age"), as.character)) |>
    pivot_longer(
      -tidyselect::any_of(
        c(
          "Park",
          "Sample_number",
          "Species",
          "Sex",
          "Age",
          "Date_of_sample_collection"
        )
      ),
      names_to = "Chemical",
      values_to = "Value"
    ) |>
    mutate(
      # Throws 2 warnings, because we use as.numeric() and grepl() on NA values,
      # but it is OK
      Detected = case_when(
        # When a cell is empty, the chemical was not detected
        is.na(Value) ~ "Not detected",
        # Not quantified values contain the "<" character
        grepl("<", Value) ~ "Detected",
        # Quantified values are values that can be converted to a numeric
        !is.na(as.numeric(Value)) ~ "Quantified"
      ),
      Value = ifelse(Detected == "Quantified", as.numeric(Value), NA)
    )

  # Assign the category to the chemicals. `chem_categories` contain everything
  # needed.
  dat_long <- left_join(dat_long, chem_categories, by = "Chemical")

  # Check for unmatched chemicals
  unmatched <- dat_long |>
    filter(is.na(primary_category)) |>
    pull(Chemical) |>
    unique()
  if (length(unmatched) > 0) {
    warning(paste(
      "Unmatched chemicals found:",
      paste(unmatched, collapse = ", ")
    ))
  }

  # Remove chemicals, where we have too little information. This means dropping
  # all chemicals, where we have no quantified values and the proportion of
  # detected values is less than 5 %.
  threshold_to_keep <- 0.05
  informative_chemicals <- dat_long |>
    count(Chemical, Detected, name = "n") |>
    pivot_wider(names_from = Detected, values_from = n, values_fill = 0) |>
    filter(Quantified > 0 | Detected >= nrow(dat) * threshold_to_keep) |>
    pull(Chemical)
  dat_long <- dat_long |> filter(Chemical %in% informative_chemicals)

  # Handle the detection of chemicals by category
  df_detected_by_category <- dat_long |>
    group_by(
      across(
        tidyselect::any_of(
          c(
            "primary_category",
            "Park",
            "Sample_number",
            "Species",
            "Sex",
            "Age",
            "Date_of_sample_collection"
          )
        )
      )
    ) |>
    summarise(
      # For plotting the descriptive concentration plot
      Value_sum_quantified_by_category = sum(Value, na.rm = TRUE),
      # For the regression model fitting
      Value_sum_by_category_left_censored = list(
        summarise_censoring(Detected, Value, Quantification_threshold)
      ),
      Detected_by_category = summarise_detection(Detected)
    ) |>
    ungroup() |>
    unnest_wider(Value_sum_by_category_left_censored)
  df_detected_by_category
}
