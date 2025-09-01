library("patchwork")
library("tidyverse")
theme_set(theme_bw())
source("functions/custom_mosaic_plot_function.R")
source("functions/ggplot_box_legend.R")
source("functions/plot_elements.R")

# Set locale to English for displaying the month names correctly
# (with fallbacks recommended by the CodeRabbit)
loc <- Sys.setlocale("LC_TIME", "English")
if (is.na(loc)) loc <- Sys.setlocale("LC_TIME", "en_US.UTF-8")
if (is.na(loc)) loc <- Sys.setlocale("LC_TIME", "English_United States.1252")

# Function converting the rgb color codes to hex for the color choice
# Can be deleted as soon as all colors are determined
rgb2hex <- function(rgbmat) {
  # function to apply to each column of input rgbmat
  process_column <- function(col) {
    rgb(rgbmat[1, col], rgbmat[2, col], rgbmat[3, col], maxColorValue = 255)
  }
  # Apply the function
  sapply(1:ncol(rgbmat), process_column)
}

# Get the plot elements ========================================================
primary_category_labels <- get_primary_category_labels()
park_labels <- get_park_labels()
park_colors <- get_park_colors()
barplot_colors <- get_barplot_colors()
sex_mosaic_colors <- get_sex_mosaic_colors()
age_mosaic_colors <- get_age_mosaic_colors()
species_mosaic_colors <- get_species_mosaic_colors()

# Read the cleaned data and convert categories to factors ======================

df_detected_by_category <- read_csv("data/data_by_pollutant_category.csv") |>
  mutate(
    # It will be ordered as Quantified < Detected < Not detected, to display
    # correctly in the mosaic plot
    Detected_by_category = factor(
      tolower(Detected_by_category),
      levels = c("quantified", "detected", "not detected"),
      ordered = TRUE
    ),
    Park = factor(
      Park,
      levels = names(park_labels),
      # ordered = TRUE for displaying in a correct order in the mosaic plots
      ordered = TRUE
    ),
    Sex = fct_relevel(factor(tolower(Sex), levels = c("male", "female"))),
    Age = factor(
      tolower(Age),
      levels = c("adult", "subadult", "fawn"),
      ordered = TRUE
    ),
    Species = factor(Species, levels = c("D. dama", "C. elaphus"))
  ) |>
  # Filter out the A60 observation, which is excluded also during the analysis
  filter(Sample_number != "A60")
dat <- read_csv("data/clean_data.csv") |>
  mutate(
    Park = factor(
      Park,
      levels = names(park_labels),
      # ordered = TRUE for displaying in a correct order in the boxplots
      ordered = TRUE
    ),
    Sex = factor(tolower(Sex), levels = c("male", "female")),
    Age = factor(
      tolower(Age),
      levels = c("adult", "subadult", "fawn"),
      ordered = TRUE
    ),
    Species = factor(
      Species,
      levels = c("D. dama", "C. elaphus")
    ),
    # First day of the month to plot the number of samples in time
    Month = floor_date(as.Date(Date_of_sample_collection), "month")
  ) |>
  # Filter out the A60 observation, which is excluded also during the analysis
  filter(Sample_number != "A60") |>
  # Convert the measurements to character to avoid problems when pivoting
  mutate(across(-c(Age, Species, Sex, Park, Month), as.character))

# Prepare the data frame for the boxplots ======================================

# Reshape the quantified observations separately
df_quantified_by_category <- filter(
  df_detected_by_category,
  Detected_by_category == "quantified"
) |>
  add_count(Park, primary_category, name = "n_quantified") |>
  mutate(Boxplot = n_quantified >= 5)

# Create the data frames for the mosaic plots ==================================

df_rectangles <- replicate(
  3,
  vector(mode = "list", length = length(primary_category_labels)),
  simplify = FALSE
)
df_mosaic <- vector(mode = "list", 3)
names(df_rectangles) <- names(df_mosaic) <- c("Sex", "Age", "Species")

for (k in seq_along(primary_category_labels)) {
  temp_filtered <- df_detected_by_category |>
    filter(primary_category == names(primary_category_labels)[k])
  for (covariate in names(df_rectangles)) {
    df_rectangles[[covariate]][[k]] <- rectangles_for_mosaic_plots(
      bars = temp_filtered$Park,
      splitting = unname(unlist(temp_filtered[, covariate])),
      sec_splitting = temp_filtered$Detected_by_category,
      bar_width_prop = 0.85
    )
  }
}

for (covariate in names(df_rectangles)) {
  df_mosaic[[covariate]] <- bind_rows(
    df_rectangles[[covariate]],
    .id = "primary_category"
  ) |>
    mutate(
      bar = factor(bar, levels = levels(dat$Park)),
      primary_category = factor(
        primary_category,
        labels = names(primary_category_labels)
      ),
      sec_split = factor(
        sec_split,
        levels = c("not detected", "detected", "quantified"),
        ordered = TRUE
      )
    )
}

# Create the bar charts ========================================================

barplots_covariates <- vector("list", 3)
names(barplots_covariates) <- c("Sex", "Age", "Species")
for (covariate in names(barplots_covariates)) {
  barplots_covariates[[covariate]] <- ggplot(
    dat,
    aes(x = Park, fill = .data[[covariate]])
  ) +
    geom_bar(
      width = 0.90,
      linewidth = 0.2,
      color = "gray10"
    ) +
    scale_fill_manual(values = barplot_colors[[covariate]]) +
    scale_x_discrete(labels = park_labels) +
    labs(title = covariate, y = "number of observations", x = "park") +
    get_barplot_descriptive_theme()
}
barplot_month <- ggplot(dat, aes(x = Month, fill = Park)) +
  geom_bar(position = position_stack(), linewidth = 0.2, color = "gray10") +
  scale_fill_manual(values = park_colors) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(
    title = "Month of sample collection",
    y = "number of observations",
    x = "month"
  ) +
  get_barplot_descriptive_theme()

barplots <- barplot_month +
  barplots_covariates$Sex +
  barplots_covariates$Age +
  barplots_covariates$Species +
  plot_layout(nrow = 2, ncol = 2, axis_titles = "collect_y")

ggsave("figure/descriptive_barplots.pdf", barplots, width = 10, height = 6.5)
ggsave(
  "figure/descriptive_barplots.png",
  barplots,
  width = 10,
  height = 6.5,
  dpi = 400,
  bg = "white"
)

# Mosaic plots =================================================================

# Set the positions of the breaks on the x axis to be in the center of the bar
x_axis_breaks <- seq(0, 1, length = length(park_labels) + 1)[-1] -
  1 / (length(park_labels) * 2)

# Mosaic plot by sex
mosaic_sex <- ggplot(
  df_mosaic$Sex,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    fill = interaction(split, sec_split),
    alpha = sec_split
  )
) +
  geom_rect() +
  scale_x_continuous(breaks = x_axis_breaks, labels = park_labels) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = paste0(seq(0, 100, by = 20), "%")
  ) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_fill_manual(
    name = "male",
    values = unlist(sex_mosaic_colors),
    breaks = names(unlist(sex_mosaic_colors["male"])),
    labels = names(sex_mosaic_colors$male)
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "park", title = "Pollutant detection by sex") +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      title = "female",
      override.aes = list(fill = sex_mosaic_colors$female)
    )
  ) +
  get_mosaicplot_theme()
ggsave("figure/mosaic_sex.pdf", mosaic_sex, width = 12, height = 8)
ggsave(
  "figure/mosaic_sex.png",
  mosaic_sex,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)


# Mosaic plot by species
mosaic_species <- ggplot(
  df_mosaic$Species,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    fill = interaction(split, sec_split),
    alpha = sec_split
  )
) +
  geom_rect() +
  scale_x_continuous(breaks = x_axis_breaks, labels = park_labels) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = paste0(seq(0, 100, by = 20), "%")
  ) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_fill_manual(
    name = "C. elaphus",
    values = unlist(species_mosaic_colors),
    breaks = names(unlist(species_mosaic_colors["C. elaphus"])),
    labels = names(species_mosaic_colors$`C. elaphus`)
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "park", title = "Pollutant detection by species") +
  guides(
    alpha = guide_legend(
      title = "D. dama",
      override.aes = list(fill = species_mosaic_colors$`D. dama`)
    )
  ) +
  get_mosaicplot_theme()
ggsave("figure/mosaic_species.pdf", mosaic_species, width = 10, height = 8)
ggsave(
  "figure/mosaic_species.png",
  mosaic_species,
  width = 10,
  height = 8,
  dpi = 400,
  bg = "white"
)

# Mosaic plot by age
mosaic_age <- ggplot(
  df_mosaic$Age,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    fill = interaction(split, sec_split),
    alpha = sec_split,
    linetype = sec_split  # Placeholder aesthetic to create the legend
  )
) +
  geom_rect() +
  scale_x_continuous(breaks = x_axis_breaks, labels = park_labels) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = paste0(seq(0, 100, by = 20), "%")
  ) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_linetype_manual(values = c(1, 1, 1)) +
  scale_fill_manual(
    name = "fawn",
    values = unlist(age_mosaic_colors),
    breaks = names(unlist(age_mosaic_colors["fawn"])),
    labels = names(age_mosaic_colors$fawn)
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "park", title = "Pollutant detection by age") +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      title = "subadult",
      override.aes = list(fill = age_mosaic_colors$subadult)
    ),
    linetype = guide_legend(
      order = 3,
      title = "adult",
      override.aes = list(fill = age_mosaic_colors$adult)
    )
  ) +
  get_mosaicplot_theme()
ggsave("figure/mosaic_age.pdf", mosaic_age, width = 12, height = 8)
ggsave(
  "figure/mosaic_age.png",
  mosaic_age,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

# Detection box- and barplots ==================================================

barplot_quantified <- ggplot(
  df_detected_by_category,
  aes(
    y = Park,
    fill = Park,
    alpha = fct_rev(Detected_by_category),
    color = Detected_by_category
  )
) +
  geom_bar(position = "fill", width = 1, linewidth = 0.2) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_discrete(limits = rev(levels(df_detected_by_category$Park))) +
  facet_grid(
    primary_category ~ .,
    switch = "y",
    labeller = as_labeller(primary_category_labels)
  ) +
  scale_color_manual(
    values = c(
      "quantified" = "gray10",
      "detected" = "gray10",
      "not detected" = alpha("white", 0)
    )
  ) +
  scale_fill_manual(values = park_colors) +
  scale_alpha_manual(
    values = c("quantified" = 1, "detected" = 0.5, "not detected" = 0)
  ) +
  labs(
    y = "",
    title = "Occurrence",
    x = "\nproportion exactly quantified\nor qualitatively detected"
  ) +
  get_barplot_detect_theme()

boxplot_quantified <- ggplot(
  df_quantified_by_category,
  aes(
    x = Value_sum_quantified_by_category,
    y = Park,
    fill = Park,
    color = Park
  )
) +
  geom_point(
    # Points for n < 5
    data = ~ subset(., !Boxplot),
    size = 1,
    shape = 1
  ) +
  # Standard boxplots with the whiskers extending to 1.5 times the interquartile
  # range
  geom_boxplot(
    data = ~ subset(., Boxplot),
    linewidth = 0.2,
    staplewidth = 1,
    width = 0.8,
    outlier.shape = 2,
    outlier.size = 1
  ) +
  scale_x_continuous(trans = "log10") +
  scale_y_discrete(limits = rev(levels(df_detected_by_category$Park))) +
  facet_grid(
    primary_category ~ .,
    switch = "y",
    labeller = as_labeller(primary_category_labels)
  ) +
  scale_color_manual(values = park_colors) +
  scale_fill_manual(values = alpha(park_colors, 0.5)) +
  labs(
    y = "",
    title = "Distribution of quantifiable concentrations",
    x = bquote("concentration in" ~ mu * "g" ~ kg^-1)
  ) +
  get_boxplot_quant_theme()

boxplot_legend <- ggplot_box_legend()

concentrations <- barplot_quantified +
  boxplot_quantified +
  boxplot_legend +
  plot_layout(
    design = "abc",
    guides = "collect",
    widths = c(1.2, 4, 2.5)
  ) &
  theme(plot.background = element_blank(), legend.position = "none")

ggsave(
  "figure/substances_quantified.pdf",
  concentrations,
  width = 8,
  height = 8
)
ggsave(
  "figure/substances_quantified.png",
  concentrations,
  width = 8,
  height = 8,
  dpi = 400,
  bg = "white"
)

# Plot of segments per sample ==================================================
df_segments <- df_detected_by_category |>
  mutate(Park = factor(Park, levels = names(park_colors))) |>
  group_by(primary_category, Park) |>
  mutate(
    ordering = 1:n() + 0.08 * as.numeric(Park)
  ) |>
  # Eliminate 4 outliers that skew the picture, in particular Z18 for the
  # Industrial chemical category and Z79, G32 and F11 for the Plasticizer
  # category
  filter(Value_min < 150) |>
  ungroup()

ggplot(
  df_segments,
  aes(
    x = ordering,
    xend = ordering,
    y = Value_min,
    yend = Value_max,
    color = Park
  )
) +
  geom_segment(linewidth = 0.1) +
  scale_color_manual(values = park_colors, labels = park_labels) +
  labs(
    title = "LOQ only",
    x = NULL,
    y = bquote("concentration in" ~ mu * "g" ~ kg^-1)
  ) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 2))
  ) +
  facet_wrap(~primary_category, nrow = 4, scales = "free_y") +
  theme(legend.key.height = unit(0.85, "cm"))
ggsave("figure/data_segments.pdf", width = 8, height = 6)
