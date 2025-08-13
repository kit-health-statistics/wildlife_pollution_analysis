#' Fit interval censored regression model by category
#'
#' @description
#' This function fits the interval regression model for all available pollutant
#' categories. (This should be 8 for the deer data, 2 for the comparison with
#' the roe deer data.)
#'
#' @param df_detected_by_category A data frame with with the interval bounds of
#'    the measurements required columns `Value_min` and `Value_max`. Required
#'    covariate columns are `Park` and `Date_of_sample_collection`. For the main
#'    analysis, covariate `Age` is also required. Metadata columns are
#'    `Sample_number`, `primary_category` and `Detected_by_category`
#' @param non_park_comparison A logical flag indicating, whether the function
#'    should perform the main analysis, or the secondary analysis using the roe
#'    deer data from outside of national parks (`non_park_comparison = TRUE`)
#' @return A list with 2 components: list of the fitted models and list of the
#'    plots
fit_interval_reg <- function(
  df_detected_by_category,
  non_park_comparison = FALSE
) {
  # Validate input (suggested by CodeRabbit) ===================================
  if (
    !is.data.frame(df_detected_by_category) ||
      nrow(df_detected_by_category) == 0
  ) {
    stop(
      "fit_interval_reg: 'df_detected_by_category' must be a non-empty data.frame"  # nolint
    )
  }
  required_cols <- c(
    "Sample_number",
    "Date_of_sample_collection",
    "Park",
    "Detected_by_category",
    "primary_category",
    "Value_min",
    "Value_max"
  )
  missing_cols <- setdiff(required_cols, names(df_detected_by_category))
  if (length(missing_cols) > 0) {
    stop(
      "fit_interval_reg: missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # We remove certain observations inside this function, even though the task is
  # quite specific, because it's done for both the main analysis and the
  # comparison with the roe deer data.
  df_detected_by_category <- df_detected_by_category |>
    # Filter out observations, where we have no date. This should be only A60.
    filter(Sample_number != "A60") |>
    mutate(
      # Convert the categorical variables to factors to keep the levels in the
      # correct order everywhere
      Park = factor(Park, levels = names(get_park_colors(non_park_comparison))),
      Detected_by_category = factor(
        Detected_by_category,
        levels = c("Quantified", "Detected", "Not detected")
      ),
      # Place the dates from different years into a single year cycle.
      # This normalization helps align seasonal patterns across different years
      # for more consistent model fitting.
      Date_of_sample_collection = unify_year(Date_of_sample_collection),
      # Convert dates to numeric values
      Date_numeric = as.numeric(Date_of_sample_collection) -
        min(as.numeric(Date_of_sample_collection))
    ) |>
    group_by(Park, primary_category) |>
    mutate(nobs = n()) |>
    ungroup() |>
    mutate(Boxplot = nobs >= 5)

  # Check for unexpected NA dates after removing known problematic samples
  if (any(is.na(df_detected_by_category$Date_of_sample_collection))) {
    warning(
      "Unexpected NA values found in Date_of_sample_collection after filtering known samples. Dataset may need re-examination."  # nolint
    )
  }

  if (!non_park_comparison) {
    # For the main deer data convert also the age variable to factor.
    df_detected_by_category <- df_detected_by_category |>
      mutate(
        Age = factor(Age, levels = c("Calf", "Subadult", "Adult"))
      )
  }

  # Fit the model for each chemical category ===================================
  if (non_park_comparison) {
    # For the roe deer data we do not have the age covariate.
    model_formula <- formula(response_surv ~ Park + pspline(Date_numeric))
  } else {
    # For the main deer data we have all covariates.
    model_formula <- formula(response_surv ~ Age + Park + pspline(Date_numeric))
  }

  category_names <- unique(df_detected_by_category$primary_category)
  mods_by_category <- plt_by_category <-
    vector(mode = "list", length = length(category_names))
  names(mods_by_category) <- names(plt_by_category) <- category_names

  for (k in seq_along(category_names)) {
    # Filter only the part corresponding to the given pollutant category
    df_filtered <- filter(
      df_detected_by_category,
      primary_category == category_names[k]
    )
    response_boundaries <- df_filtered |>
      dplyr::select(Value_min, Value_max) |>
      as.matrix()
    response_surv <- Surv(
      time = response_boundaries[, 1],
      time2 = response_boundaries[, 2],
      type = "interval2"
    )

    # Fit
    mods_by_category[[k]] <- survreg(
      model_formula,
      data = df_filtered,
      dist = "lognormal",
      control = list(iter = 500)
    )

    # Plot results (If throws one warning
    # "Removed 1 row containing missing values"), everything is fine.
    plt_by_category[[k]] <- plot_results(
      df_filtered,
      mods_by_category[[k]],
      category_names[k],
      non_park_comparison = non_park_comparison
    )
  }
  ret <- list(fitted_mods = mods_by_category, plt = plt_by_category)
  ret
}
