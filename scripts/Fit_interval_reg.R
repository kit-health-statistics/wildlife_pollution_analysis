library("tidyverse")
library("survival")
library("patchwork")
source("functions/fit_interval_regression.R")
source("functions/plot_elements.R")
source("functions/helper_functions.R")
source("functions/plot_results.R")
theme_set(theme_bw())

df_detected_by_category <- read_csv("data/data_by_pollutant_category.csv")

results <- fit_interval_reg(df_detected_by_category)

# Save the plots
save_results_as_image(results$plt)

# Save the results into an Excel file
wb <- save_results_as_xls(results$fitted_mods)
openxlsx::saveWorkbook(wb, "tables/model_summaries.xlsx", overwrite = TRUE)

# Diagnostics
fits_vs_residuals <- results$fitted_mods |>
  lapply(function(x) {
    data.frame(fitted = fitted(x), residuals = residuals(x))
  }) |>
  bind_rows(.id = "Category")
ggplot(fits_vs_residuals, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue") +
  facet_wrap(~Category, scales = "free")
