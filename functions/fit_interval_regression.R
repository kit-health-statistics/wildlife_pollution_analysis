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
#' @param return_plots A logical flag indicating, whether the plots should be
#'    generated
#' @param intercept A logical flag indicating, whether to include the intercept
#'    in the graphical display of the spline curve
#' @param centered A logical flag indicating, whether to center the spline
#'    curve in the graphical display
#' @return A list with 2 components: list of the fitted models and list of the
#'    plots
fit_interval_reg <- function(
  df_detected_by_category,
  non_park_comparison = FALSE,
  return_plots = TRUE,
  intercept = FALSE,
  centered = TRUE
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
    mutate(
      # Convert the categorical variables to factors to keep the levels in the
      # correct order everywhere
      Park = factor(Park, levels = names(get_park_colors(non_park_comparison))),
      Detected_by_category = factor(
        Detected_by_category,
        levels = c("quantified", "detected", "not detected")
      ),
      # Place the dates from different years into a single year cycle.
      # This normalization helps align seasonal patterns across different years
      # for more consistent model fitting.
      Date_of_sample_collection = unify_year(Date_of_sample_collection),
      # Convert dates to numeric values
      Date_numeric = as.numeric(Date_of_sample_collection) -
        min(as.numeric(Date_of_sample_collection))
    )

  if (!non_park_comparison) {
    # For the main deer data convert also the age variable to factor.
    df_detected_by_category <- df_detected_by_category |>
      mutate(
        Age = factor(Age, levels = c("fawn", "subadult", "adult"))
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

  # Categories, that we decided not to include in the analysis, because we have
  # too little data, the model is too sensitive to the assumptions etc.
  excluded_categories <- get_excluded_categories()

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
    fit <- try(
      survreg(
        model_formula,
        data = df_filtered,
        dist = "lognormal",
        control = list(iter = 500)
      ),
      silent = TRUE
    )

    # Catch errors. If a model was impossible to fit for one of the excluded
    # categories, we are fine. If the fitting failed for at least one "good"
    # category we proceed anyway to get results at least for other categories.
    if (inherits(fit, "try-error")) {
      if (category_names[k] %in% excluded_categories) {
        message(paste0(category_names[k], " category was not possible to fit."))
      } else {
        warning(
          paste0(category_names[k], " category was not possible to fit.")
        )
      }
      mods_by_category[[k]] <- list()
      plt_by_category[[k]] <- list()
    } else {
      mods_by_category[[k]] <- fit
      # Plot results (If throws one warning
      # "Removed 1 row containing missing values"), everything is fine.
      if (return_plots) {
        plt_by_category[[k]] <- plot_results(
          df_filtered,
          mods_by_category[[k]],
          category_names[k],
          non_park_comparison = non_park_comparison,
          intercept = intercept,
          centered = centered
        )
      } else {
        plt_by_category[[k]] <- list()
      }
    }
  }
  ret <- list(fitted_mods = mods_by_category, plt = plt_by_category)
  ret
}
