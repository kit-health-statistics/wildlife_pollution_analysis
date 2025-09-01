# A set of wrapper functions defining graphical elements of the plots

# Define text labels ===========================================================

# Category of the pollutants to display
get_primary_category_labels <- function() {
  c(
    "Anthropogenic pollution" = "Anthropogen. \npollution",
    "API" = "API",
    "Industrial chemical" = "Industrial chemical",
    "PAH" = "PAH",
    "PCP" = "PCP",
    "Pesticide" = "Pesticide",
    "POP" = "POP",
    "Plasticizer" = "Plasticizer"
  )
}

# Park names to display
get_park_labels <- function(non_park_comparison = FALSE) {
  labs <- c(
    "Bay_Wald" = "Bayerischer \nWald",
    "Eifel" = "Eifel",
    "Hainich" = "Hainich",
    "Hunsrueck" = "Hunsrück\nHochwald",
    "Jasmund" = "Jasmund",
    "Kellerwald" = "Kellerwald\nEdersee",
    "Saechs_Schw" = "Sächsische \nSchweiz",
    "Vorpomm" = "Vorpomm. \nBoddenldsch."
  )
  if (non_park_comparison) {
    labs <- c(labs, "Non-Park" = "non-Park")
  }
  labs
}

# Define colors ================================================================

# Color coding of parks in the concentrations plot
get_park_colors <- function(non_park_comparison = FALSE) {
  cols <- c(
    "Bay_Wald" = "deepskyblue4",
    "Eifel" = "#AD7B00",
    "Hainich" = "firebrick3",
    "Hunsrueck" = "purple4",
    "Jasmund" =  "#5E6B7B",
    "Kellerwald" = "salmon4",
    "Saechs_Schw" = "#004000",
    "Vorpomm" = "magenta3"
  )
  if (non_park_comparison) {
    cols <- c(cols, "Non-Park" = "seagreen4")
  }
  cols
}

# Color coding of the covariate categories in descriptive barplots
get_barplot_colors <- function() {
  list(
    Sex = c("male" = "royalblue2", "female" = "firebrick3"),
    Age = c(
      "fawn" = "palevioletred2",
      "subadult" = "#46CD8A",
      "adult" = "darkorange3"
    ),
    Species = c("C. elaphus" = "orangered4", "D. dama" = "goldenrod2")
  )
}

# Color coding for the mosaic plot by sex
get_sex_mosaic_colors <- function() {
  list(
    male = c(
      "not detected" = "skyblue1",
      "detected" = "royalblue2",
      "quantified" = "navyblue"
    ),
    female = c(
      "not detected" = "#FF9999",
      "detected" = "firebrick2",
      "quantified" = "firebrick4"
    )
  )
}

# Color coding for the mosaic plot by species
get_species_mosaic_colors <- function() {
  list(
    `C. elaphus` = c(
      "not detected" = "lightcoral",
      "detected" = "brown3",
      "quantified" = "orangered4"
    ),
    `D. dama` = c(
      "not detected" = "khaki2",
      "detected" = "goldenrod2",
      "quantified" = "darkgoldenrod4"
    )
  )
}

# Color coding for the mosaic plot by age
get_age_mosaic_colors <- function() {
  list(
    fawn = c(
      "not detected" = "pink",
      "detected" = "palevioletred2",
      "quantified" = "hotpink4"
    ),
    subadult = c(
      "not detected" = "palegreen",
      "detected" = "#46CD8A",
      "quantified" = "darkgreen"
    ),
    adult = c(
      "not detected" = "#FFBA80",
      "detected" = "darkorange2",
      "quantified" = "sienna4"
    )
  )
}

# Define plot themes ===========================================================

# Theme of the descriptive barplots
get_barplot_descriptive_theme <- function() {
  theme(
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
}

# Theme of the descriptive mosaic plots
get_mosaicplot_theme <- function() {
  theme(
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
}

# Theme of the barplots displaying detection categories
get_barplot_detect_theme <- function() {
  theme(
    plot.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0),
    panel.spacing = unit(.1, "cm"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0, size = 12),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, -0.1, "cm")
  )
}

get_boxplot_quant_theme <- function() {
  theme(
    strip.text.y.left = element_blank(),
    panel.spacing = unit(.1, "cm"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0, size = 12),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, -0.1, "cm")
  )
}

# Define linewidth for the result-plotting from the model fit ==================
get_p_val_cat_linewidth <- function() {
  c(
    "reference" = 0.2,
    "(0,0.01]" = 1.4,
    "(0.01,0.05]" = 1,
    "(0.05,0.1]" = 0.6,
    "(0.1,1]" = 0.2,
    "-1" = 0
  )
}
