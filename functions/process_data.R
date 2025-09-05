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
#' @param exclude_uninformative A logical flag, indicating whether to drop
#'    uninformative substances from the list of measured chemicals. We want
#'    to exclude them from the model fitting, but keep them for the descriptive
#'    plots. See ‘Details’.
#' @return A data frame with aggregated measurements by category including
#'    detection status and interval bounds for best/worst-case
#'    scenarios
#' @details If \code{exclude_uninformative = TRUE}, all chemicals are dropped
#'    from the analysis, for which:
#'    \begin{itemize}
#'      \item no "quantified" value was found and at the same time
#'      \item the proportion of the "detected" values is less than 5 %
#'    \end{itemize}
#'
#'    Overall detection status:
#'    \begin{itemize}
#'      \item "quantified": At least one sample in a category contains a
#'      quantified value.
#'      \item "detected": No sample contains a quantified value and at least one
#'      sample in a category contains a detected value under the quantification
#'      limit.
#'      \item "not detected": All samples are non-detects.
#'    \end{itemize}
#'    Definition of the best/worst-cases:
#'    \begin{itemize}
#'      \item For all samples containing only non-detects or non-quantifiable
#'      values, the best case is a (near) 0 value and the worst case is the sum
#'      of the quantification thresholds.
#'      \item When at least value is quantified, the non-detects and
#'      non-quantifiable ones are resolved as above and then the sum of all
#'      quantifiable values is added to the both best and worst case values.
#'    \end{itemize}
process_data <- function(dat, chem_categories, exclude_uninformative = TRUE) {
  # Non-measurement columns that shall be excluded from some dplyr processing
  # operations
  non_measurement_colomns <- c(
    "Park",
    "Sample_number",
    "Species",
    "Sex",
    "Age",
    "Date_of_sample_collection"
  )

  # Reshape the data to a long format
  dat_long <- dat |>
    # Convert the measurements to character to avoid problems when pivoting
    mutate(
      across(-tidyselect::any_of(non_measurement_colomns), as.character)
    ) |>
    pivot_longer(
      -tidyselect::any_of(non_measurement_colomns),
      names_to = "Chemical",
      values_to = "Value"
    ) |>
    mutate(
      detected = case_when(
        # When a cell is empty, the chemical was not detected
        is.na(Value) ~ "not detected",
        # Not quantified values contain the "<" character
        grepl("<", Value) ~ "detected",
        # Quantified values are values that can be converted to a numeric.
        # We suppress warnings, because some of the `Value` values are NAs,
        # representing the non-detects, which then make `as.numeric()` to throw
        # a warning, even though everything is fine.
        !is.na(suppressWarnings(as.numeric(Value))) ~ "quantified"
      ),
      # If not detected, the cell contains value in a format "<LOQ". We extract
      # these stated LOQs from here, so that we can compare them later with
      # those contained in the list of substances and discover discrepancies.
      Stated_threshold = ifelse(
        detected == "detected",
        as.numeric(str_sub(Value, start = 2)),
        NA_real_
      ),
      # Convert the character values to numeric values for the quantified
      # measurements.
      Value = ifelse(
        detected == "quantified",
        suppressWarnings(as.numeric(Value)),
        NA_real_
      )
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

  # Check that all the quantified values are above the LOQ. If there is a
  # quantified measurement below the LOQ, it indicates an error in the data.
  below_LOQ <- dat_long |>  # nolint
    mutate(
      below_LOQ = (detected == "quantified" & Value < Quantification_threshold)
    ) |>
    pull(below_LOQ)
  if (any(below_LOQ)) {
    sample_string <- str_c(dat_long$Sample_number[below_LOQ], collapse = ", ")
    substance_string <- str_c(dat_long$Chemical[below_LOQ], collapse = ", ")
    warning(paste0("There are quantified measurements below the LOQ. In particular for samples " , sample_string, " and substances ", substance_string, ".\n"))  # nolint
  }

  # Check that the LOQs stated in the individual measurements match with those
  # in the list of substances
  threshold_mismatch <- dat_long |>
    mutate(
      threshold_mismatch = detected == "detected" &
        Stated_threshold != Quantification_threshold
    ) |>
    pull(threshold_mismatch)
  if (any(threshold_mismatch)) {
    sample_string <- str_c(
      dat_long$Sample_number[threshold_mismatch],
      collapse = ", "
    )
    substance_string <- str_c(
      dat_long$Chemical[threshold_mismatch],
      collapse = ", "
    )
    warning(paste0("There are mismatches between the LOQs in the data and in the list of the substances. In particular for samples " , sample_string, " and substances ", substance_string, ".\n"))  # nolint
  }

  # Remove chemicals, where we have too little information. This means dropping
  # all chemicals, where we have no quantified values and the proportion of
  # detected values is less than 5 %.
  # Inside an if statement, because we want to keep the excluded substances
  # for the descriptive plots.
  if (exclude_uninformative) {
    threshold_to_keep <- 0.05
    informative_chemicals <- dat_long |>
      count(Chemical, detected, name = "n") |>
      pivot_wider(names_from = detected, values_from = n, values_fill = 0) |>
      filter(quantified > 0 | detected >= nrow(dat) * threshold_to_keep) |>
      pull(Chemical)
    dat_long <- dat_long |> filter(Chemical %in% informative_chemicals)
  }

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
        summarise_censoring(detected, Value, Quantification_threshold)
      ),
      Detected_by_category = summarise_detection(detected)
    ) |>
    ungroup() |>
    unnest_wider(Value_sum_by_category_left_censored)

  # If we do not exclude the uninformative sample, that means we want to draw
  # the descriptive bar- and boxplots. We order the factor levels here, so that
  # they are displayed correctly in all figures.
  if (!exclude_uninformative) {
    df_detected_by_category <- df_detected_by_category |>
      mutate(
        # It will be ordered as quantified < detected < not detected, to display
        # correctly in the mosaic plot
        Detected_by_category = factor(
          Detected_by_category,
          levels = c("quantified", "detected", "not detected"),
          ordered = TRUE
        ),
        Park = factor(
          Park,
          levels = names(get_park_labels()),
          # ordered = TRUE for displaying in a correct order in the mosaic plots
          ordered = TRUE
        ),
        Sex = factor(Sex, levels = c("male", "female")),
        Age = factor(
          Age,
          levels = c("adult", "subadult", "fawn"),
          ordered = TRUE
        ),
        Species = factor(Species, levels = c("D. dama", "C. elaphus"))
      ) |>
      # Indicate whether to draw a boxplot, or individual observations.
      # For a boxplot we need at least 5 observations.
      group_by(primary_category, Park) |>
      mutate(n_quantified = sum(Detected_by_category == "quantified")) |>
      mutate(Boxplot = n_quantified >= 5)
  }

  # Return
  df_detected_by_category
}
