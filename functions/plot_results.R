# Function plotting the overview of the regression fit
plot_results <- function(
  df_filtered,
  fitted_survreg_model,
  pollutant_category,
  all_plots = FALSE
) {
  # List to store the plots
  plt <- vector("list", 6)
  names(plt) <- c("spline", "age", "park", "boxplot", "barplot", "composite")

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

  # Plot results
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
    geom_point(
      data = df_filtered,
      aes(x = df_filtered$Date_of_sample_collection, y  = 0),
      shape = 1
    ) +
    labs(
      x = "Date",
      title = "Penalized spline for the date variable (intercept included)",
      y = NULL  # Maybe add label "Concentration [correct unit]"?
    )

  plt$age <- plot_reg_coeffs(
    "Age",
    levels(df_filtered$Age),
    coeffs,
    summ,
    category_on_x = FALSE
  ) +
    scale_y_discrete(labels = levels(df_filtered$Age))

  plt$park <- plot_reg_coeffs("Park", levels(df_filtered$Park), coeffs, summ) +
    # `park_labels` from the "plot_elements.R" file
    scale_x_discrete(labels = park_labels)

  plt$boxplot <- df_filtered |>
    filter(
      Detected_by_category == "Quantified"
    ) |>
    ggplot(
      aes(
        y = Value_sum_by_category,
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
    scale_color_manual(values = park_colors) +
    scale_x_discrete(labels = park_labels, drop = FALSE) +
    scale_fill_manual(values = park_colors) +
    labs(x = "Park", y = "Concentration", title = "Quantified concentrations")

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
      )
    ) +
    scale_fill_manual(
      values = park_colors,
      labels = park_labels
    ) +
    scale_alpha_manual(
      breaks = c("Quantified", "Detected"),
      values = c("Quantified" = 1, "Detected" = 0.5, "Not detected" = 0)
    ) +
    labs(title = "Detections")

  # Compose the figures using patchwork
  plt$composite <-
    (plt$spline + plt$age + plt$park + plt$boxplot + plt$barplot) +
    plot_layout(
      design = c("ab \n cc \n dd \n ee"),
      heights = c(2, 1, 2),
      widths = c(4, 1)
    ) &
    theme(legend.position = "none") &  # Add legend later
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
