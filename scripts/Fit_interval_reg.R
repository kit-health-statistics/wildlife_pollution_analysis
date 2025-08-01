library("tidyverse")
library("survival")
library("patchwork")
source("scripts/plot_elements.R")
source("functions/helper_functions.R")
source("functions/plot_results.R")
theme_set(theme_bw())

# Load the cleaned data
df_detected_by_category <- read_csv("data/data_by_pollutant_category.csv") |>
  # Filter out an observation, where we have no date
  filter(!is.na(Date_of_sample_collection)) |>
  # Filter out the first observation in time, which is too far away from the
  # others
  filter(Date_of_sample_collection != as.Date("2024-05-29")) |>
  mutate(
    # Convert the categorical variables to factors to keep the levels in the
    # correct order everywhere
    Age = factor(Age, levels = c("Calf", "Subadult", "Adult")),
    Park = factor(Park, levels = names(park_colors)),
    Detected_by_category = factor(
      Detected_by_category,
      levels = c("Quantified", "Detected", "Not detected")
    ),
    # Push the later dates one year back to close the gap between the data
    # points. 1. April seems to be a good cutoff point
    Date_of_sample_collection = as.Date(
      ifelse(
        Date_of_sample_collection > as.Date("2024-04-01"),
        Date_of_sample_collection - 366,  # 2024 was a leap year
        Date_of_sample_collection
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

category_names <- unique(df_detected_by_category$primary_category)
mods_by_category <- plt_by_category <-
  vector(mode = "list", length = length(category_names))
names(mods_by_category) <- category_names


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
    response_surv ~ Age + Park + pspline(Date_numeric),
    data = df_filtered,
    dist = "lognormal",
    control = list(iter = 500)
  )

  # Plot results (If throws one warning
  # "Removed 1 row containing missing values"), everything is fine.
  plt_by_category[[k]] <- plot_results(
    df_filtered,
    mods_by_category[[k]],
    names(primary_category_labels)[k]
  )
  ggsave(
    paste0(
      "figure/Results_visualization_",
      names(primary_category_labels)[k],
      ".pdf"
    ),
    plt_by_category[[k]],
    width = 10,
    height = 9
  )
}

# Diagnostics
fits_vs_residuals <- mods_by_category |>
  lapply(function(x) {
    data.frame(fitted = fitted(x), residuals = residuals(x))
  }) |>
  bind_rows(.id = "Category")
ggplot(fits_vs_residuals, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue") +
  facet_wrap(~Category, scales = "free")
