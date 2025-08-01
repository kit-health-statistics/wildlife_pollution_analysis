# A snippet defining graphical elements of the descriptive plots

# Define text labels ===========================================================

# Category of the pollutants to display
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

# Park names to display
park_labels <- c(
  "Bay_Wald" = "Bayerischer \nWald",
  "Hainich" = "Hainich",
  "Hunsrueck" = "Hunsrück\nHochwald",
  "Saechs_Schw" = "Sächsische \nSchweiz",
  "Jasmund" = "Jasmund",
  "Kellerwald" = "Kellerwald\nEdersee",
  "Eiffel" = "Eiffel",
  "Vorpomm" = "Vorpomm. \nBoddenlandschaft"
)

# Define colors ================================================================

# Color coding of parks in the concentrations plot
park_colors <- c(
  "Bay_Wald" = "deepskyblue4",
  "Hainich" = "firebrick3",
  "Hunsrueck" = "purple3",
  "Saechs_Schw" = "darkgreen",
  "Jasmund" = "lightblue3",
  "Kellerwald" = "palegreen",
  "Eiffel" = "goldenrod1",
  "Vorpomm" = "magenta3"
)

# Color coding of the covariate catgories in desriptive barplots
barplot_colors <- list(
  Sex = c("Male" = "royalblue2", "Female" = "firebrick3"),
  Age = c(
    "Calf" = "palevioletred2",
    "Subadult" = "#46CD8A",
    "Adult" = "darkorange3"
  ),
  Species = c("C. elaphus" = "orangered4", "D. dama" = "goldenrod2"),
  Season = c(
    "Summer 2024" = "coral",
    "Winter 2024/25" = "skyblue2",
    "Winter 2023/24" = "steelblue4"
  )
)

# Color coding for the mosaic plot by sex
sex_mosaic_colors <- list(
  Male = c(
    "Not detected" = "skyblue1",
    "Detected" = "royalblue2",
    "Quantified" = "navyblue"
  ),
  Female = c(
    "Not detected" = "#FF9999",
    "Detected" = "firebrick2",
    "Quantified" = "firebrick4"
  )
)

# Color coding for the mosaic plot by species
species_mosaic_colors <- list(
  `C. elaphus` = c(
    "Not detected" = "lightcoral",
    "Detected" = "brown3",
    "Quantified" = "orangered4"
  ),
  `D. dama` = c(
    "Not detected" = "khaki2",
    "Detected" = "goldenrod2",
    "Quantified" = "darkgoldenrod4"
  )
)

# Color coding for the mosaic plot by age
age_mosaic_colors <- list(
  Calf = c(
    "Not detected" = "pink",
    "Detected" = "palevioletred2",
    "Quantified" = "hotpink4"
  ),
  Subadult = c(
    "Not detected" = "palegreen",
    "Detected" = "#46CD8A",
    "Quantified" = "darkgreen"
  ),
  Adult = c(
    "Not detected" = "#FFBA80",
    "Detected" = "darkorange2",
    "Quantified" = "sienna4"
  )
)

# Define plot themes ===========================================================

# Theme of the descriptive barplots
barplot_desciptive_theme <- theme(
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

# Theme of the descriptive mosiaic plots
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

# Theme of the barplots displaying detection categories
barplot_detection_theme <- theme(
  plot.background = element_blank(),
  strip.placement = "outside",
  strip.text.y.left = element_text(angle = 0),
  panel.spacing = unit(.1,"cm"),
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

boxplot_quantification_theme <- theme(
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

# Define linewidth for the result-plotting from the model fit ==================
p_val_cat_linewidth <- c(
  "reference" = 0.2,
  "(0,0.01]" = 1.4,
  "(0.01,0.05]" = 1,
  "(0.05,0.1]" = 0.6,
  "(0.1,1]" = 0.2
)
