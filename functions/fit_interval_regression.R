# Function fitting the interval regression model for all available pollutant
# categories. (This should be 8 for the deer data, 2 for the comparison with the
# roe data.)
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

  # Load the cleaned data ======================================================

  # We remove certain observations inside this function, when though the task is
  # quite specific, because it's done for both the main analysis and the
  # comparison with the roe deer data.
  df_detected_by_category <- df_detected_by_category |>
    # Filter out observations, where we have no date. This should be only A60.
    filter(!is.na(Date_of_sample_collection)) |>
    # Filter out the first observation in time, which is too far away from the
    # others. This is Z91.
    filter(Date_of_sample_collection != as.Date("2024-05-29")) |>
    mutate(
      # Convert the categorical variables to factors to keep the levels in the
      # correct order everywhere
      Park = factor(Park, levels = names(get_park_colors(non_park_comparison))),
      Detected_by_category = factor(
        Detected_by_category,
        levels = c("Quantified", "Detected", "Not detected")
      ),
      # Push the dates from 2024 one year back to close the gap between the data
      # points. 1. April seems to be a good cutoff point. The roe deer data from
      # 2021 will be pushed forward by 2 years.
      # This normalization helps align seasonal patterns across different years
      # for more consistent model fitting.
      Date_of_sample_collection = as.Date(
        case_when(
          Date_of_sample_collection > as.Date("2024-04-01") ~
            Date_of_sample_collection - 366,  # 2024 was a leap year
          Date_of_sample_collection <= as.Date("2021-07-09") ~
            Date_of_sample_collection + 2 * 365,  # For the roe deer data only
          .default = Date_of_sample_collection
        )
      ),
      # Convert dates to numeric values
      Date_numeric = as.numeric(Date_of_sample_collection) -
        min(as.numeric(Date_of_sample_collection))
    ) |>
    group_by(Park, primary_category) %>%
    mutate(nobs = n()) %>%
    ungroup() %>%
    mutate(Boxplot = nobs >= 5)
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
    response_boundaries <- df_filtered %>%
      dplyr::select(Value_min, Value_max) %>%
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
