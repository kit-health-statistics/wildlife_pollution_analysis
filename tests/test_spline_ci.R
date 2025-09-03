library("tidyverse")
library("survival")
source(here::here("functions", "fit_interval_regression.R"))
source(here::here("functions", "helper_functions.R"))
source(here::here("functions", "plot_elements.R"))

test_that("Manual calculation of the spline fit is correct", {

  # Load the data
  df_detected_by_category <- read_csv(
    here::here("data", "data_by_pollutant_category.csv")
  ) |>
    filter(Sample_number != "Z91")

  # What is the time difference between the first and the last observation?
  timeline_length <- df_detected_by_category$Date_of_sample_collection |>
    unify_year() |>
    range() |>
    diff() |>
    as.numeric()

  # Fit the model
  results <- fit_interval_reg(df_detected_by_category, return_plots = FALSE)

  # New data frame for the `predict()` function
  newdata <- data.frame(
    Date_numeric = seq(from = 0, to = timeline_length, by = 1),
    Park = "Bay_Wald",
    Age = "fawn"
  )
  for (k in seq_along(results$fitted_mods)) {
    fit_auto <- predict(results$fitted_mods[[k]], newdata = newdata, se = TRUE)
    fit_manual <- calculate_spline_ci(
      results$fitted_mods[[k]],
      timeline_length,
      intercept = TRUE
    )
    fit_manual_centered <- calculate_spline_ci(
      results$fitted_mods[[k]],
      timeline_length,
      intercept = FALSE
    )$fit
    fit_auto_centered <- fit_auto$fit /
      exp(coefficients(results$fitted_mods[[k]])["(Intercept)"])

    # Skip tests, if fitting not possible
    if (!anyNA(fit_auto$fit)) {
      # Check the calculation comparing the non-centered versions including the
      # SEs
      expect_lt(max(abs(fit_auto$fit - fit_manual$fit)), 1e-11)
      expect_lt(max(abs(fit_auto$se.fit - fit_manual$se)), 1e-11)

      # Check the centering using the centered version, fits only
      expect_lt(max(abs(fit_auto_centered - fit_manual_centered)), 1e-11)
    }
  }
})
