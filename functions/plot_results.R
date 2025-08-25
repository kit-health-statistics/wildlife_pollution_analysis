# Function plotting the overview of the regression fit
plot_results <- function(
  df_filtered,
  fitted_survreg_model,
  pollutant_category,
  all_plots = FALSE,
  non_park_comparison = FALSE
) {
  # List to store the plots
  plt <- vector("list", 5)
  names(plt) <- c("spline", "reg_coeffs", "boxplot", "barplot", "composite")

  # Get the plot elements
  park_labels <- get_park_labels(non_park_comparison)
  park_colors <- get_park_colors(non_park_comparison)

  # Extract the coefficients and standard errors
  coeffs <- coefficients(fitted_survreg_model)
  summ <- summary(fitted_survreg_model)

  # Display the spline =========================================================

  # For calculating the fitted spline curve
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
    Age = "Fawn"  # Reference category
  )
  spline_curve <- as.data.frame(
    predict(fitted_survreg_model, newdata = newdata, se = TRUE)
  ) |>
    mutate(
      Date_of_sample_collection = newdata$Date_of_sample_collection,
      lower = fit - se.fit,
      upper = fit + se.fit
    )

  # Data frames for the rest of the plots (coefficient tiles, boxplots and
  # barplots) ==================================================================
  x_labels_coeffs <- park_labels
  names(x_labels_coeffs) <- names(park_labels)

  df_park <- extract_reg_coeffs(
    "Park",
    levels(df_filtered$Park),
    coeffs,
    summ
  )

  # If we compare with the roe deer data from non-park localities, we have to
  # drop the age covariate from all plots
  if (non_park_comparison) {
    df_coeffs <- df_park
    covcat_colors <- park_colors
    coeff_plot_title <- "Park regression coefficients"
    # Add the two placeholder columns for consistency with the age+park path.
    df_boxbar <- df_filtered |>
      mutate(Covariate = "Park", Covariate_category = Park)
  } else {
    # Number of empty tiles between the times for parks and age categories
    empty_tiles <- 1
    df_age <- extract_reg_coeffs(
      "Age",
      levels(df_filtered$Age),
      coeffs,
      summ
    )
    df_empty <- data.frame(
      Vals = NA,
      p_val = NA,
      p_val_label = NA,
      p_val_cat = -1,
      lower = NA,
      upper = NA,
      formatted_coeff_label = NA,
      coeff = paste0("Empty_", 1:empty_tiles),
      # a y value to plot the tiles in the ggplot coordinates
      dummy_value = 1,
      empty = "empty"
    )
    df_coeffs <- rbind(df_park, df_empty, df_age)

    covcat_colors <- c(
      park_colors,
      rep(NA, empty_tiles),
      get_age_mosaic_colors() |>
        lapply(function(x) unname(x["Quantified"])) |>
        unlist()
    )
    names(covcat_colors) <- df_coeffs$coeff
    x_labels_coeffs <- c(x_labels_coeffs, levels(df_filtered$Age))
    names(x_labels_coeffs) <- c(names(park_labels), levels(df_filtered$Age))
    coeff_plot_title <- "Park regression coefficients"

    # For the descriptive box- and barplot, concatenate the Park and Age
    # covariates
    df_boxbar <- df_filtered |>
      pivot_longer(
        c(Park, Age),
        names_to = "Covariate",
        values_to = "Covariate_category"
      )
  }

  df_boxbar <- df_boxbar |>
    # Set factor levels for the correct ordering of bars
    mutate(
      Covariate_category = factor(
        Covariate_category,
        levels = names(covcat_colors)
      )
    ) |>
    # Indicate, whether we have enough observations to draw a boxplot
    group_by(Covariate_category) |>
    mutate(n_quantified = sum(Detected_by_category == "Quantified")) |>
    ungroup() |>
    mutate(Boxplot = n_quantified >= 5)

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
    ) +
    coord_cartesian(ylim = c(0, NA))

  # Breaks of the color gradient for the categorical coefficients. Needs to be
  # determined manually.
  colorbar_breaks <- c(0.0015, 0.5, 1, 1.65, 2.3)
  plt$reg_coeffs <- ggplot() +
    geom_tile(
      data = df_coeffs,
      mapping = aes(
        x = coeff,
        y = dummy_value,
        fill = Vals,
        linewidth = p_val_cat,
        color = empty
      )
    ) +
    geom_text(
      data = subset(df_coeffs, p_val_cat != "reference"),
      mapping = aes(
        x = coeff,
        y = dummy_value + 0.75,  # Add a value to be just above the boxes/tiles
        label = p_val_label
      ),
      size = 3
    ) +
    geom_text(
      data = df_coeffs,
      mapping = aes(x = coeff, y = dummy_value, label = formatted_coeff_label),
      size = 3
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Coefficient values\n(higher = more polluted)",
      title = coeff_plot_title
    ) +
    scale_fill_gradient2(
      low = "royalblue",
      midpoint = 1,
      high = "firebrick2",
      na.value = alpha("white", 0),
      breaks = colorbar_breaks,
      labels = colorbar_breaks,
      limits = range(colorbar_breaks)
    ) +
    scale_x_discrete(
      breaks = names(x_labels_coeffs),
      labels = x_labels_coeffs
    ) +
    # `p_val_cat_linewidth` from the "plot_elements.R" file
    scale_linewidth_manual(values = get_p_val_cat_linewidth(), guide = "none") +
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

  plt$boxplot <- df_boxbar |>
    filter(
      Detected_by_category == "Quantified"
    ) |>
    ggplot(
      aes(
        y = Value_sum_quantified_by_category,
        x = Covariate_category,
        fill = Covariate_category,
        color = Covariate_category
      )
    ) +
    geom_point(
      # Points for n < 5
      data = ~ subset(., !Boxplot),
      size = 2,
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
    scale_color_manual(values = covcat_colors, guide = "none") +
    scale_x_discrete(
      breaks = names(x_labels_coeffs),
      labels = x_labels_coeffs,
      drop = FALSE
    ) +
    scale_y_continuous(
      trans = "log10",
      breaks = c(1, 10, 100, 1000)
    ) +
    scale_fill_manual(values = covcat_colors, guide = "none") +
    labs(
      x = NULL,
      y = bquote("Concentration in" ~ mu * "g" ~ kg^-1),
      title = "Quantified concentrations"
    ) +
    coord_cartesian(ylim = c(1, 1000)) +
    theme(axis.text.x = element_text(size = 8))

  plt$barplot <- ggplot(
    df_boxbar,
    aes(
      x = Covariate_category,
      fill = Covariate_category,
      alpha = fct_rev(Detected_by_category),
      color = Detected_by_category
    )
  ) +
    geom_bar(position = "fill", width = 0.8, linewidth = 0.2) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    scale_x_discrete(
      breaks = names(x_labels_coeffs),
      labels = x_labels_coeffs,
      drop = FALSE
    ) +
    scale_color_manual(
      values = c(
        "Quantified" = "gray10",
        "Detected" = "gray10",
        "Not detected" = alpha("white", 0)
      ),
      guide = "none"
    ) +
    scale_fill_manual(
      values = covcat_colors,
      guide = "none"
    ) +
    scale_alpha_manual(
      breaks = c("Quantified", "Detected"),
      values = c("Quantified" = 1, "Detected" = 0.5, "Not detected" = 0),
      name = "Occurrence\nof pollutants"
    ) +
    labs(
      title = "Proportion quantified or qualitatively detected",
      y = NULL,
      x = NULL
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 8)
    )

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
