library("patchwork")
library("tidyverse")
theme_set(theme_bw())
source("functions/custom_mosaic_plot_function.R")
source("functions/ggplot_box_legend.R")

# Functions for nice plotting ==================================================

# Function converting the rgb color codes to hex for the color choice
# Can be deleted as soon as all colors are determined
rgb2hex <- function(rgbmat){
  # function to apply to each column of input rgbmat
  ProcessColumn = function(col){
    rgb(rgbmat[1, col],
        rgbmat[2, col],
        rgbmat[3, col],
        maxColorValue = 255)
  }
  # Apply the function
  sapply(1:ncol(rgbmat), ProcessColumn)
}

# Plot the boxes with whiskers
boxplot_custom <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# Define the labels, colors and themes used in the plots =======================

primary_category_labels <- c(
  "Anthropogenic pollution" = "Anthropogen. \npollution",
  "API" = "API",
  "Industrial chemical" = "Industrial chemical",
  "PAH" = "PAH",
  "PCP" = "PCP",
  "Pesticide" = "Pesticide",
  "POP" = "POP",
  "Plasticizer" = "Plasticizer"
)

park_labels <- c(
  "Bay_Wald" = "Bayerischer \nWald",
  "Hainich" = "Hainich",
  "Hunsrueck" = "Hunsrück\nHochwald",
  "Saechs_Schw" = "Sächsische \nSchweiz",
  "Jasmund" = "Jasmund",
  "Harz" = "Harz",
  "Kellerwald" = "Kellerwald\nEdersee",
  "Eiffel" = "Eiffel",
  "Vorpomm" = "Vorpomm. \nBoddenlandschaft"
)

park_colors <- c(
  "Bay_Wald" = "deepskyblue4",
  "Hainich" = "firebrick3",
  "Saechs_Schw" = "darkgreen",
  "Hunsrueck" = "purple3",
  "Jasmund" = "lightblue3",
  "Harz" = "salmon4",
  "Kellerwald" = "palegreen",
  "Eiffel" = "goldenrod1",
  "Vorpomm" = "magenta3"
)

barplot_colors <- list(
  Sex = c("Male" = "royalblue2", "Female" = "firebrick3"),
  Age = c("Calf" = "palevioletred2", "Subadult" = "#46CD8A", 
          "Adult" = "darkorange3"),
  Species = c("C. elaphus" = "orangered4", "D. dama" = "goldenrod2"),
  Season = c("Summer 2024" = "coral", "Winter 2024/25" = "skyblue2",
             "Winter 2023/24" = "steelblue4")
)

barplot_theme <- theme(
  legend.position = "top",
  panel.grid.major.x = element_blank(),
  plot.title = element_text(hjust = 0.5, size = 12),
  legend.title = element_blank(),
  legend.key.width = unit(0.3, "cm"),
  legend.key.height = unit(0.3, "cm"),
  panel.grid.minor.x = element_blank(),
  strip.background = element_blank(),
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)

mosaicplot_theme <- theme(
  plot.title = element_text(hjust = 0.5),
  legend.key.width = unit(0.3, "cm"),
  legend.key.height = unit(0.3, "cm"),
  axis.text = element_text(size = 6),
  axis.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank()
)

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
      levels = c("Bay_Wald", "Hainich", "Hunsrueck", "Saechs_Schw", "Jasmund", 
                 "Harz", "Kellerwald", "Eiffel", "Vorpomm"), 
      ordered = TRUE  # TRUE for displaying in a correct order in the mosaic plots
    ),
    Sex = factor(Sex, levels = c("Male", "Female")),
    Age = factor(Age, levels = c("Adult", "Subadult", "Calf"), ordered = TRUE),
    Species = factor(Species, levels = c("D. dama", "C. elaphus")),
    Season = factor(Season, levels = c("Summer 2024", "Winter 2024/25", "Winter 2023/24"))
  )
dat <- read_csv("data/clean_data.csv") %>%
   mutate(
     Park = factor(
       Park, 
       levels = c("Bay_Wald", "Hainich", "Hunsrueck", "Saechs_Schw", 
                  "Jasmund", "Harz", "Kellerwald", "Eiffel", "Vorpomm"),
       ordered = TRUE  # TRUE for displaying in a correct order in the boxplots
       ),
     Sex = factor(Sex, levels = c("Male", "Female")),
     Age = factor(Age, levels = c("Adult", "Subadult", "Calf"), ordered = TRUE),
     Species = factor(Species, levels = c("C. capreolus", "D. dama", "C. elaphus")),
     Season = factor(Season, levels = c("Summer 2024", "Winter 2024/25", "Winter 2023/24"))
   ) %>% # Convert the measurements to character to avoid problems when pivoting
  mutate_at(vars(-Age, -Species, -Sex, -Season, -Park), as.character)

# Prepare the data frame for the boxplots ======================================

# Reshape the quantified observations separately
df_quantified_by_category <- filter(
  df_detected_by_category,
  Detected_by_category == "Quantified"
) %>% group_by(Park, primary_category) %>%
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
      primary_category = factor(primary_category, labels = names(primary_category_labels)),
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
    barplot_theme
}
barplot_season <- ggplot(dat, aes(x = Park, fill = Season)) +
  geom_bar(position = position_stack(), width = 0.90) +
  scale_fill_manual(values = barplot_colors$Season) +
  scale_x_discrete(labels = park_labels) +
  labs(y = "", title = "Season of sample collection") +
  barplot_theme

barplots <- barplot_season + barplots_covariates$Sex + 
  barplots_covariates$Age + barplots_covariates$Species +
  plot_layout(nrow = 2, ncol = 2)

ggsave("figure/descriptive_barplots.pdf", barplots, width = 10, height = 6.5)

# Mosaic plots =================================================================

# Set the positions of the breaks on the x axis to be in the centre of the bar
x_axis_breaks <- seq(0, 1, length = length(park_labels) + 1)[-1] - 1 / (length(park_labels) * 2)

# Mosaic plot by species
mosaic_species <- ggplot(
  df_mosaic$Species,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = interaction(split, sec_split), alpha = sec_split)
) +
  geom_rect() +
  scale_x_continuous(breaks = x_axis_breaks, labels = park_labels) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%")) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_fill_manual(
    name = "C. elaphus",
    values = c(
      "C. elaphus.Not detected" = "lightcoral",
      "C. elaphus.Detected" = "brown3",
      "C. elaphus.Quantified" = "orangered4",
      "D. dama.Not detected" = "khaki2",
      "D. dama.Detected" = "goldenrod2",
      "D. dama.Quantified"  = "darkgoldenrod4"
    ),
    breaks = c("C. elaphus.Not detected", "C. elaphus.Detected", "C. elaphus.Quantified"),
    labels = c("Not detected", "Detected", "Quantified")
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "Park", y = "Species", title = "Pollutant detection by species") +
  guides(
    alpha = guide_legend(
      title = "D. dama",
      override.aes = list(
        fill = c("Not detected" = "khaki2", "Detected" = "goldenrod2", "Quantified" = "darkgoldenrod4")
      )
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
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%")) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_linetype_manual(values = c(1, 1, 1)) +
  scale_fill_manual(
    name = "Calf",
    values = c(
      "Calf.Not detected" = "pink",
      "Calf.Detected" = "palevioletred2",
      "Calf.Quantified"  = "hotpink4",
      "Subadult.Not detected" = "palegreen",
      "Subadult.Detected" = "#46CD8A",
      "Subadult.Quantified" = "darkgreen",
      "Adult.Not detected" = "#FFBA80",
      "Adult.Detected" = "darkorange2",
      "Adult.Quantified" = "sienna4"
      ),
    breaks = c("Calf.Not detected", "Calf.Detected", "Calf.Quantified"),
    labels = c("Not detected", "Detected", "Quantified")
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "Park", title = "Pollutant detection by age") +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      title = "Subadult",
      override.aes = list(
        fill = c("Not detected" = "palegreen", "Detected" = "#46CD8A", "Quantified" = "darkgreen")
      )
    ),
    linetype = guide_legend(
      order = 3,
      title = "Adult",
      override.aes = list(
        fill = c("Not detected" = "#FFBA80", "Detected" = "#FF8830", "Quantified" = "sienna4")
      )
    )
  ) +
  mosaicplot_theme
ggsave("figure/mosaic_age.pdf", mosaic_age, width = 12, height = 8)

# Mosaic plot by sex
mosaic_sex <- ggplot(
  df_mosaic$Sex,
  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = interaction(split, sec_split), alpha = sec_split)
  ) +
  geom_rect() +
  scale_x_continuous(breaks = x_axis_breaks, labels = park_labels) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%")) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_fill_manual(
    name = "Male",
    values = c(
      "Male.Not detected" = "skyblue1",
      "Female.Not detected" = "#FF9999",
      "Male.Detected" = "royalblue2",
      "Female.Detected" = "firebrick2",
      "Male.Quantified"  = "navyblue",
      "Female.Quantified" = "firebrick4"
      ),
    breaks = c("Male.Not detected", "Male.Detected", "Male.Quantified"),
    labels = c("Not detected", "Detected", "Quantified")
  ) +
  facet_wrap(~primary_category, nrow = 4, ncol = 2) +
  labs(x = "Park", title = "Pollutant detection by sex") +
  guides(
    fill = guide_legend(order = 1),
    alpha = guide_legend(
      order = 2,
      title = "Female",
      override.aes = list(
        fill = c("Not detected" = "#FF9999", "Detected" = "firebrick2", "Quantified" = "firebrick4")
      )
    )
  ) +
  mosaicplot_theme
ggsave("figure/mosaic_sex.pdf", mosaic_sex, width = 12, height = 8)

# Detectection box- and barplots =============================================

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
  facet_grid(primary_category~., switch = "y", labeller = as_labeller(primary_category_labels)) +
  scale_color_manual(
    values = c("Quantified" = "gray10", "Detected" = "gray10", "Not detected" = alpha("white", 0)),
    guide = "none"
  ) +
  scale_fill_manual(
    name = "National park",
    values = park_colors,
    labels = park_labels
  ) +
  scale_alpha_manual(
    name = "Left panel:",
    breaks = c("Quantified", "Detected"),
    values = c("Quantified" = 1, "Detected" = 0.5, "Not detected" = 0),
    labels = c("Quantified" = "Quantified", "Detected" = "Detected only \nQualitatively")
  ) +
  labs(y = "", title = "Occurence", x = "proportion exactly quantified \n or qualitatively detected") +
  guides(
    fill = guide_legend(order = 1),  # National parks first
    alpha = guide_legend(
      order = 2,
      title = "Occurence \n of pollutants\n(left panel):",
      override.aes = list(
        fill = c("Quantified" = "gray20", "Detected" = "gray60"),
        alpha = c("Quantified" = 1, "Detected" = 1),
        color = c("Quantified" = "gray10", "Detected" = "gray10"),
        linewidth = c("Quantified" = 0.2, "Detected" = 0.2)
      )
    )
  ) +
  theme(plot.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0),
    panel.spacing = unit(.1,"cm"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0, size = 12),
    legend.key.width = unit(0.6, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, -0.1, "cm")
  )

boxplot_quantified <- ggplot(
  df_quantified_by_category,
  aes(
    x = Value_sum_by_category, 
    y = Park, 
    fill = Park, 
    color = Park, 
    shape = placeholder
    )
  ) +
  geom_point(  # Points for n < 5
    data =  ~ subset(., !Boxplot),
    size = 1
  ) +
  stat_summary(  # Boxplot for nobs >= 5
    data = ~ subset(., Boxplot),
    fun.data = boxplot_custom,
    linewidth = 0.2,
    geom = "boxplot",
    key_glyph = "point",
    staplewidth = 1,
    width = 0.8
  ) +
  scale_x_continuous(trans = "log10") +
  scale_y_discrete(limits = rev(levels(df_detected_by_category$Park))) +
  facet_grid(primary_category~., switch = "y", labeller = as_labeller(primary_category_labels)) +
  scale_shape_manual(values = 1, breaks = "placeholder", labels = "Individual values \nif n < 5",
                     guide = "none") +  # Uncomment if needed
  scale_color_manual(
    values = park_colors,
    guide = "none"
  ) +
  scale_fill_manual(
    values = alpha(park_colors, 0.5),
    guide = "none"
  ) +
  labs(
    y = "",
    title = "Distribution of quantifiable concentrations",
    x = bquote("concentration in"~mu*"g"~kg^-1)
  ) +
  guides(
    # shape = guide_legend(
    #   title = "Concentration\n(right panel):",
    #   override.aes = list(color = c("placeholder" = "gray20"), shape = 1, size = 1.5)
    # )
  ) +
  theme(
    strip.text.y.left = element_blank(),
    panel.spacing = unit(.1,"cm"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0, size = 12),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, -0.1, "cm")
  )

boxplot_legend <- ggplot_box_legend()

part <- barplot_quantified + boxplot_quantified

design <- "abc \n abd"

concentrations <- part + guide_area() + boxplot_legend +
  plot_layout(
    design = design,
    guides = "collect",
    widths = c(1.2, 4, 2.5),
    heights = c(2, 1.4)
  ) &
  theme(plot.background = element_blank())

ggsave("figure/substances_quantified.pdf", concentrations, width = 8, height = 8)
