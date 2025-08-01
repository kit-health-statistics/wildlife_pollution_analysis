library("patchwork")
library("tidyverse")
theme_set(theme_bw())
source("functions/custom_mosaic_plot_function.R")
source("functions/ggplot_box_legend.R")
source("scripts/plot_elements.R")

# Functions for nice plotting ==================================================

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

# Read the cleaned data and convert categories to factors ======================

df_detected_by_category <- read_csv("data/data_by_pollutant_category.csv") %>%
  mutate(
    # It will be ordered as Quantified < Detected < Not detected, to display
    # correctly in the mosaic plot
    Detected_by_category = factor(
      Detected_by_category,
      levels = c("Quantified", "Detected", "Not detected"),
      ordered = TRUE
    ),
    Park = factor(
      Park,
      levels = names(park_labels),
      # ordered = TRUE for displaying in a correct order in the mosaic plots
      ordered = TRUE
    ),
    Sex = factor(Sex, levels = c("Male", "Female")),
    Age = factor(Age, levels = c("Adult", "Subadult", "Calf"), ordered = TRUE),
    Species = factor(Species, levels = c("D. dama", "C. elaphus")),
    Season = factor(
      Season,
      levels = c("Summer 2024", "Winter 2024/25", "Winter 2023/24")
    )
  )
dat <- read_csv("data/clean_data.csv") %>%
  mutate(
    Park = factor(
      Park,
      levels = names(park_labels),
      # ordered = TRUE for displaying in a correct order in the boxplots
      ordered = TRUE
    ),
    Sex = factor(Sex, levels = c("Male", "Female")),
    Age = factor(Age, levels = c("Adult", "Subadult", "Calf"), ordered = TRUE),
    Species = factor(
      Species,
      levels = c("D. dama", "C. elaphus")
    ),
    Season = factor(
      Season,
      levels = c("Summer 2024", "Winter 2024/25", "Winter 2023/24")
    )
  ) %>%
  # Convert the measurements to character to avoid problems when pivoting
  mutate_at(vars(-Age, -Species, -Sex, -Season, -Park), as.character)

# Prepare the data frame for the boxplots ======================================

# Reshape the quantified observations separately
df_quantified_by_category <- filter(
  df_detected_by_category,
  Detected_by_category == "Quantified"
) %>%
  group_by(Park, primary_category) %>%
  mutate(nobs = n()) %>%
  ungroup() %>%
  mutate(Boxplot = nobs >= 5, placeholder = "placeholder")

# Create the data frames for the mosaic plots ==================================

df_rectangles <- replicate(
  3,
  vector(mode = "list", length = length(primary_category_labels)),
  simplify = FALSE
)
df_mosaic <- vector(mode = "list", 3)
names(df_rectangles) <- names(df_mosaic) <- c("Sex", "Age", "Species")

for (k in seq_along(primary_category_labels)) {
  temp_filtered <- df_detected_by_category %>%
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
  ) %>%
    mutate(
      bar = factor(bar, levels = levels(dat$Park)),
      primary_category = factor(
        primary_category,
        labels = names(primary_category_labels)
      ),
      sec_split = factor(
        sec_split,
        levels = c("Not detected", "Detected", "Quantified"),
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
    geom_bar(position = "fill", width = 0.90) +
    scale_fill_manual(values = barplot_colors[[covariate]]) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.2),
      labels = paste0(seq(0, 100, by = 20), "%")
    ) +
    scale_x_discrete(labels = park_labels) +
    labs(y = "", title = covariate) +
    barplot_desciptive_theme
}
barplot_season <- ggplot(dat, aes(x = Park, fill = Season)) +
  geom_bar(position = position_stack(), width = 0.90) +
  scale_fill_manual(values = barplot_colors$Season) +
  scale_x_discrete(labels = park_labels) +
  labs(y = "", title = "Season of sample collection") +
  barplot_desciptive_theme

barplots <- barplot_season +
  barplots_covariates$Sex +
  barplots_covariates$Age +
  barplots_covariates$Species +
  plot_layout(nrow = 2, ncol = 2)

ggsave("figure/descriptive_barplots.pdf", barplots, width = 10, height = 6.5)

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
    name = "Male",
    values = unlist(sex_mosaic_colors),
    breaks = names(unlist(sex_mosaic_colors["Male"])),
    labels = names(sex_mosaic_colors$Male)
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "Park", title = "Pollutant detection by sex") +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      title = "Female",
      override.aes = list(fill = sex_mosaic_colors$Female)
    )
  ) +
  mosaicplot_theme
ggsave("figure/mosaic_sex.pdf", mosaic_sex, width = 12, height = 8)


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
  labs(x = "Park", y = "Species", title = "Pollutant detection by species") +
  guides(
    alpha = guide_legend(
      title = "D. dama",
      override.aes = list(fill = species_mosaic_colors$`D. dama`)
    )
  ) +
  mosaicplot_theme
ggsave("figure/mosaic_species.pdf", mosaic_species, width = 10, height = 8)

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
    linetype = sec_split  # Placeholder aestetic to create the legend
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
    name = "Calf",
    values = unlist(age_mosaic_colors),
    breaks = names(unlist(age_mosaic_colors["Calf"])),
    labels = names(age_mosaic_colors$Calf)
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "Park", title = "Pollutant detection by age") +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      title = "Subadult",
      override.aes = list(fill = age_mosaic_colors$Subadult)
    ),
    linetype = guide_legend(
      order = 3,
      title = "Adult",
      override.aes = list(fill = age_mosaic_colors$Adult)
    )
  ) +
  mosaicplot_theme
ggsave("figure/mosaic_age.pdf", mosaic_age, width = 12, height = 8)

# Detection box- and barplots ==================================================

barplot_quantified <- ggplot(
  df_detected_by_category,
  aes(
    y = Park,
    fill = Park,
    alpha = fct_relevel(Detected_by_category, rev),
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
      "Quantified" = "gray10",
      "Detected" = "gray10",
      "Not detected" = alpha("white", 0)
    )
  ) +
  scale_fill_manual(values = park_colors) +
  scale_alpha_manual(
    values = c("Quantified" = 1, "Detected" = 0.5, "Not detected" = 0)
  ) +
  labs(
    y = "",
    title = "Occurence",
    x = "\nProportion exactly quantified\nor qualitatively detected"
  ) +
  barplot_detection_theme

boxplot_quantified <- ggplot(
  df_quantified_by_category,
  aes(
    x = Value_sum_by_category,
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
    x = bquote("Concentration in" ~ mu * "g" ~ kg^-1)
  ) +
  boxplot_quantification_theme

source("functions/ggplot_box_legend.R")
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
