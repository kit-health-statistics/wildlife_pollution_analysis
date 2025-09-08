# Function for creating a visual definition/explanation of a boxplot.
# Adapted from a blogpost:
# "Exploring ggplot2 boxplots - Defining limits and adjusting style"
# by Laura DeCicco
# https://waterdata.usgs.gov/blog/boxplots/
ggplot_box_legend <- function(boxplot_only = FALSE, family = "sans") {

  # Get the predefined plot elements ===========================================
  park_colors <- get_park_colors()
  park_labels <- get_park_labels()

  # Set up the parameters guiding the size, position and distance ==============

  # The text label positions need some fiddling to get them just right.
  # Must be adjusted, when the aspect ratio of the final figure changes.

  # Where items should be on the x axis
  labels_position_x <- 1  # Legend items
  titles_position_x <- 0.85  # Legend titles
  glyphs_position_x <- 0.9  # Key glyphs (squares and the boxplot)

  # Where the legend titles should be on the y axis
  labels_position_y <- list("Park" = 3500, "Detection" = 1800, "Boxplot" = 1000)

  # The legend for parks and the detection types is represented by colored
  # squares. This value specifies, how far away are the square centers from each
  # other on the vertical axis.
  square_distance <- 150

  # Prepare the data for plotting  =============================================

  # Create data to draw the boxplot
  set.seed(100)
  sample_data <- sample.int(500)
  # Extend the top whisker a bit:
  sample_data[1:100] <- 701:800
  # There will be 2 lower outliers
  outliers <- data.frame(
    x = glyphs_position_x,
    y = c(-310, -460),
    Point_type = c("outlier", "individual_val")
  )

  # Segments to mark the interquartile range (IQR). 2 horizontal segments,
  # 1 vertical.
  quartiles <- quantile(sample_data, c(0.25, 0.5, 0.75))
  IQR <- quartiles[3] - quartiles[1]  # nolint
  df_IQR_segment <- data.frame(  # nolint
    x = c(1.3, 1.3, 1.45),
    y = quartiles[c(1, 3, 1)],
    xend = rep(1.45, 3),
    yend = quartiles[c(1, 3, 3)]
  )

  # Set the coordinates of the text labels =====================================

  # Calculate coordinates, where to place text labels of the boxplot
  df_boxplot_labels <- data.frame(
    x = c(
      rep(labels_position_x, 7),
      1.35,  # IQR label
      titles_position_x  # Title
    ),
    y = c(
      quartiles,
      # Upper whisker. Shift the label a little bit below
      max(sample_data[sample_data < (quartiles[3] + 1.5 * IQR)]) - 50,
      # Lower whisker. Shift the label a little bit below
      min(sample_data[sample_data > (quartiles[1] - 1.5 * IQR)]) - 50,
      outliers$y,
      quartiles[2],  # IQR label
      labels_position_y$Boxplot  # Title
    ),
    Labels = c(
      "25th percentile",
      "median",
      "75th percentile",
      "largest value within\n1.5 x IQR above\n75th percentile",  # Whisker
      "smallest value within\n1.5 x IQR below\n25th percentile",  # Whisker
      "outliers",
      "individual values\nif n < 5",
      "IQR",
      "Concentration \n(right panel):"  # Title
    ),
    Size = c(rep("Label_text", 8), "Title")
  )

  if (boxplot_only) {
    df_labels <- df_boxplot_labels
    # Rewrite the boxplot legend title, which relates to the big descriptive
    # graphic on default
    df_labels[df_labels$Labels == "Concentration \n(right panel):", "Labels"] <-
      "Concentration:"
  } else {
    # Where to draw the squares representing the fill colors of parks and
    # the type of detection. The squares will be drawn using
    # `geom_point(shape = 2)` with a large size instead of `geom_rect()`.
    df_rect <- data.frame(
      x = glyphs_position_x,
      y = c(
        # Draw from the title downwards
        labels_position_y$Park - (seq_along(park_labels) * square_distance),
        # Draw from the title downwards and add some space between the title and
        # the legend items, as the title for the detection has multiple lines
        labels_position_y$Detection -
          (seq_len(2) * square_distance) -
          0.5 * square_distance
      ),
      fill_color = c(names(park_colors), c("quantified", "detected"))
    )

    # Retrieve coordinates, where to place text labels of the parks and
    # detection categories
    df_rect_labels <- data.frame(
      x = c(
        # Legend item labels: 8 for parks, 2 for detection category
        rep(labels_position_x, length(park_labels) + 2),
        # 2 titles
        rep(titles_position_x, 2)
      ),
      # y labels match the centers of the squares, the last 2 are for the titles
      y = c(df_rect$y, labels_position_y$Park, labels_position_y$Detection),
      Labels = c(
        unname(park_labels),
        "quantified",
        "detected only\nqualitatively",
        "National park",  # Title
        "Occurrence\nof pollutants\n(left panel):"  # Title
      ),
      Size = c(rep("Label_text", length(park_labels) + 2), rep("Title", 2))
    )
    df_labels <- rbind(df_boxplot_labels, df_rect_labels)
  }

  # Create the legend ==========================================================
  explain_plot <- ggplot() +
    geom_boxplot(
      aes(x = glyphs_position_x, y = sample_data),
      width = 0.1,
      fill = "lightgrey",
      outlier.shape = 1,
      staplewidth = 1
    ) +
    geom_point(
      data = outliers,
      mapping = aes(x = x, y = y, shape = Point_type),
      size = 1.5
    ) +
    geom_segment(
      data = df_IQR_segment,
      mapping = aes(x = x, xend = xend, y = y, yend = yend)
    ) +
    geom_text(
      data = df_labels,
      mapping = aes(x = x, y = y, label = Labels, size = Size),
      vjust = 0.4,
      hjust = 0,
      lineheight = 0.9
    ) +
    scale_shape_manual(values = c("outlier" = 2, "individual_val" = 1)) +
    scale_size_manual(values = c("Label_text" = 2.8, "Title" = 3.8)) +
    theme_void(base_family = family) +
    theme(
      legend.position = "none"
    )

  if (!boxplot_only) {
    explain_plot <- explain_plot +
      # Square legend items
      geom_point(
        data = df_rect,
        aes(x = x, y = y, fill = fill_color),
        size = 8,
        shape = 22
      ) +
      # Fill colors for the square legend items
      scale_fill_manual(
        values = c(park_colors, "quantified" = "gray10", "detected" = "gray60")
      )
  }

  explain_plot
}
