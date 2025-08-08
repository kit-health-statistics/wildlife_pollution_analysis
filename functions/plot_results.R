# Function plotting the overview of the regression fit
plot_results <- function(
  df_filtered,
  fitted_survreg_model,
  pollutant_category,
  all_plots = FALSE
) {
  # List to store the plots
  plt <- vector("list", 6)
  names(plt) <- c("spline", "reg_coeffs", "boxplot", "barplot", "composite")

  # Extract the coefficients and standard errors
  coeffs <- coefficients(fitted_survreg_model)
  summ <- summary(fitted_survreg_model)

  # For displaying the fitted spline curve
  newdata <- data.frame(
    Date_numeric = seq(
      from = min(df_filtered$Date_numeric),
      to = max(df_filtered$Date_numeric),
      by = 1
    ),
    Date_of_sample_collection = seq(
      from = min(df_filtered$Date_of_sample_collection),
      to = max(df_filtered$Date_of_sample_collection),
      by = 1
    ),
    Park = "Bay_Wald",  # Reference category
    Age = "Calf"  # Reference category
  )
  spline_curve <- as.data.frame(
    predict(fitted_survreg_model, newdata = newdata, se = TRUE)
  ) |>
    mutate(
      Date_of_sample_collection = newdata$Date_of_sample_collection,
      lower = fit - se.fit,
      upper = fit + se.fit
    )

  # Number of empty tiles between the times for parks and age categories
  empty_tiles <- 1
  df_park <- extract_reg_coeffs(
    "Park",
    levels(df_filtered$Park),
    coeffs,
    summ
  )
  df_age <- extract_reg_coeffs(
    "Age",
    levels(df_filtered$Age),
    coeffs,
    summ
  )
  df_empty <- data.frame(
    Vals = NA,
    p_val = NA,
    p_val_cat = -1,
    formatted_label = NA,
    coeff = paste0("Empty_", 1:empty_tiles),
    # a y value to plot the tiles in the ggplot coordinates
    dummy_value = 1,
    empty = "empty"
  )
  df_coeffs <- rbind(df_park, df_empty, df_age)

  x_labels_coeffs <- c(
    park_labels,
    levels(df_filtered$Age)
  )
  names(x_labels_coeffs) <- c(
    names(park_labels),
    levels(df_filtered$Age)
  )

  # Plot results ===============================================================
  plt$spline <- ggplot() +
    geom_line(
      data = spline_curve,
      aes(x = Date_of_sample_collection, y = fit)
    ) +
    geom_ribbon(
      data = spline_curve,
      aes(x = Date_of_sample_collection, ymin = lower, ymax = upper),
      alpha = 0.5
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%d %b") +
    labs(
      x = "Date",
      title = "Penalized spline for the date variable (intercept included)",
      y = bquote("Concentration in" ~ mu * "g" ~ kg^-1)
    )

  colorbar_breaks <- c(0.005, 0.07, 1, 9, 80)
  plt$reg_coeffs <- ggplot(
    df_coeffs,
    aes(
      x = coeff,
      y = dummy_value,
      fill = Vals,
      label = formatted_label,
      linewidth = p_val_cat,
      color = empty
    )
  ) +
    geom_tile() +
    geom_text(size = 3) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Coefficient values\n(higher = more polluted)",
      title = "Park and age regression coefficients"
    ) +
    scale_fill_gradient2(
      low = "firebrick2",
      midpoint = 1,
      high = "royalblue",
      na.value = alpha("white", 0),
      breaks = colorbar_breaks,
      labels = colorbar_breaks,
      limits = range(colorbar_breaks),
      transform = "log"
    ) +
    scale_x_discrete(
      breaks = names(x_labels_coeffs),
      labels = x_labels_coeffs
    ) +
    # `p_val_cat_linewidth` from the "plot_elements.R" file
    scale_linewidth_manual(values = p_val_cat_linewidth, guide = "none") +
    scale_color_manual(
      values = c("non-empty" = "black", "empty" = alpha("white", 0)),
      guide = "none"
    ) +
    coord_equal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8)
    )

  plt$boxplot <- df_filtered |>
    filter(
      Detected_by_category == "Quantified"
    ) |>
    ggplot(
      aes(
        y = Value_sum_quantified_by_category,
        x = Park,
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
    geom_boxplot(
      data = ~ subset(., Boxplot),
      staplewidth = 1,
      width = 0.8,
      linewidth = 0.2,
      alpha = 0.5,
      outlier.shape = 2
    ) +
    scale_color_manual(values = park_colors, guide = "none") +
    scale_x_discrete(labels = park_labels, drop = FALSE) +
    scale_fill_manual(values = park_colors, guide = "none") +
    labs(
      x = NULL,
      y = bquote("Concentration in" ~ mu * "g" ~ kg^-1),
      title = "Quantified concentrations"
    )

  plt$barplot <- ggplot(
    df_filtered,
    aes(
      x = Park,
      fill = Park,
      alpha = fct_relevel(Detected_by_category, rev),
      color = Detected_by_category
    )
  ) +
    geom_bar(position = "fill", width = 0.8, linewidth = 0.2) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    scale_x_discrete(labels = park_labels, drop = FALSE) +
    scale_color_manual(
      values = c(
        "Quantified" = "gray10",
        "Detected" = "gray10",
        "Not detected" = alpha("white", 0)
      ),
      guide = "none"
    ) +
    scale_fill_manual(
      values = park_colors,
      labels = park_labels,
      guide = "none"
    ) +
    scale_alpha_manual(
      breaks = c("Quantified", "Detected"),
      values = c("Quantified" = 1, "Detected" = 0.5, "Not detected" = 0),
      name = "Occurence\nof pollutants"
    ) +
    labs(title = "Proportion quantified or qualitatively detected", y = NULL)

  # Compose the figures using patchwork
  plt$composite <-
    (plt$spline / plt$reg_coeffs / plt$boxplot / plt$barplot) +
    plot_layout(
      guides = "collect"
    ) &
    theme(legend.position = "right") &
    plot_annotation(
      title = pollutant_category,
      theme = theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          lineheight = 0.9
        )
      )
    )
  if (all_plots) {
    # Return all individual plots
    return(plt)
  } else {
    # Return only the composite plot
    return(plt$composite)
  }
}
