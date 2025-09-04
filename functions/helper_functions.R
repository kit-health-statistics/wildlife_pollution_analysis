# Functions for data cleaning ==================================================

# Relabeling function for the names of the species. Input is the character
# vector of the species names.
relabel_species <- function(x) {
  case_when(
    x %in% c("Rotwild", "Rothirsch") ~ "C. elaphus",
    x %in% c("Damwild", "Damhirsch") ~ "D. dama"
  )
}

# Relabeling function for the sex categories. During the loading from Excel,
# some escape characters are read. Input is the character vector of sex
# indicators.
relabel_sex <- function(x) {
  case_when(
    x %in% c("männlich", "männlich\r\n") ~ "male",
    x %in% c("weiblich", "weiblich\r\n") ~ "female"
  )
}

# Relabeling function for the age categories. Input is the character
# vector of the age category names.
relabel_age <- function(x) {
  case_when(
    x == "adult" ~ "adult",
    x == "subadult" ~ "subadult",
    x == "Kalb" ~ "fawn"
  )
}

# Function to create the age category from a value indicated by the parks.
# Values indicated by the parks are either numeric (Jasmund and
# Vorpomm. Boddenlandshaft), or character strings (Sächsische Schweiz).
# The resulting age category is already present in the dataset, but this
# function serves for verifying that the columns match, as the categorization
# was likely done  manually.
# The 'age' value comes in as a character string. 'age_numeric' indicates if the
# string represents a numeric value or a categorical value.
age_category_from_extra_column <- function(age, age_numeric = TRUE) {
  if (age_numeric) {
    age <- as.numeric(age)
    case_when(
      age == 0.5 ~ "fawn",
      age > 0.5 & age <= 1.5 ~ "subadult",
      age > 1.5 ~ "adult"
    )
  } else {
    case_when(
      age %in% c("Altersklasse 0", "Altersklasse 0\r\n") ~ "fawn",
      age %in% c("Altersklasse 1", "Altersklasse 1\r\n") ~ "subadult",
      age %in% c("Altersklasse 2", "Altersklasse 2\r\n") ~ "adult"
    )
  }
}

# Function renaming columns that are either misspelled, or inconsistently named
# across data sheets. It takes a data frame as the argument. All the problematic
# names were determined beforehand manually. Input is the data frame containing
# the raw data.
rename_columns <- function(x) {
  if ("Flupyram" %in% colnames(x)) {
    x <- x |> rename("Fluopyram" = "Flupyram")
  }
  if ("Uvinul® A plus" %in% colnames(x)) {
    x <- x |> rename("Uvinul A Plus" = "Uvinul® A plus")
  }
  if ("Estron" %in% colnames(x)) {
    x <- x |> rename("Estrone" = "Estron")
  }
  if ("Terbutylazine" %in% colnames(x)) {
    x <- x |> rename("Terbuthylazine" = "Terbutylazine")
  }
  if ("Terbutylazine-desethyl" %in% colnames(x)) {
    x <- x |> rename("Terbuthylazine-desethyl" = "Terbutylazine-desethyl")
  }
  if ("Tebuconazol" %in% colnames(x)) {
    x <- x |> rename("Tebuconazole" = "Tebuconazol")
  }
  if ("Tetraconazol" %in% colnames(x)) {
    x <- x |> rename("Tetraconazole" = "Tetraconazol")
  }
  if ("2,4,4'-Trichlorobiphenyl (PCB #52)" %in% colnames(x)) {
    x <- x |>
      rename(
        # Determined the correct name after an exchange with Michelle
        "2,2',5,5'-Tetrachlorobiphenyl (PCB #52)" = "2,4,4'-Trichlorobiphenyl (PCB #52)"  # nolint
      )
  }
  # Return
  x
}

# Functions for data processing ================================================

#' Function determining the overall detection status of a chemical category
#'
#' @param x A character vector of possible values "not detected", "detected",
#'   "quantified" for individual chemicals
#' @return A string indicating the aggregated status of "not detected",
#'   "detected" and "quantified" for the whole category of chemical substances
#' @details Overall detection status:
#'    \begin{itemize}
#'      \item "quantified": At least one sample in a category contains a
#'      quantified value.
#'      \item "detected": No sample contains a quantified value and at least one
#'      sample in a category contains a detected value under the quantification
#'      limit.
#'      \item "not detected": All samples are non-detects.
#'    \end{itemize}
summarise_detection <- function(x) {
  if (all(x == "not detected")) {
    # Category was not detected, when no chemicals from the category were
    # detected
    ret <- "not detected"
  } else if (any(x == "quantified")) {
    # Category was quantified, when at least one chemical from the category was
    # quantified
    ret <- "quantified"
  } else {
    # Category was detected, when at least one chemicals from the category was
    # qualitatively detected and no chemicals from the category were quantified
    ret <- "detected"
  }
  ret
}

#' Function creating a range for the aggregate concentration value per chemical
#' category
#'
#' @description This function creates a range per chemical category, where the
#'    sum of all observed values lie. This accounts for the censoring when a
#'    chemical is detected only qualitatively, or not detected at all.
#' @param detected A character vector indicating detection with possible values
#'    "Not detected", "Detected", "Quantified" for individual chemicals
#' @param value A numeric vector containing the concentration values for the
#'    quantified measurements and NA for the rest
#' @param threshold A numeric vector containing the quantification thresholds of
#'    individual chemicals
#' @return A named numeric vector of length 2 with elements:
#'    \code{Value_min} and \code{Value_max} defining the lower and upper bounds
#'    (best and worst case values) for the aggregated concentration in the
#'    category
#' @details When a chemical was not detected, or not quantified we consider
#'    these observations to be anywhere between 0 and the quantification
#'    threshold.
#'    Definition of the aggregated value range:
#'    \begin{itemize}
#'      \item For all samples containing only non-detects or non-quantifiable
#'      values, the best case is 0 and the worst case is the sum of the
#'      quantification thresholds (LOQ).
#'      \item When at least one value is quantified, the best value is the sum
#'      of all quantified values and then the sum of the quantification
#'      thresholds is added to account for non-detects and non-quantifiable
#'      values.
#'    \end{itemize}
summarise_censoring <- function(detected, value, threshold) {
  vals_sum <- sum(value[detected == "quantified"], na.rm = TRUE)
  ret <- c(
    Value_min = vals_sum,
    Value_max = vals_sum + sum(threshold[detected != "quantified"])
  )
  ret
}

#' Function for removing information on year
#'
#' @description This function sets the year component of a date to an (almost)
#'    arbitrary year 2022/2023 (2023 for January and February, 2022 for the
#'    rest). The only non-arbitrary aspect is that 2023 was not a leap year.
#'    This is not a concern, since we never get observations from 29 February.
#' @param date_vec A vector of values in a date format
#' @return A vector of values in a date format with the year set to 2022, or
#'    2023
unify_year <- function(date_vec) {
  str_m_d <- paste(
    stringr::str_pad(month(date_vec), pad = "0", side = "left", width = 2),
    stringr::str_pad(day(date_vec), pad = "0", side = "left", width = 2),
    sep = "-"
  )
  # Cut off at the 8th month, so that we have the beginning in August and the
  # end in winter for the deer data, or in July for the roe deer data.
  str_y <- ifelse(month(date_vec) < 8, "2023", "2022")

  as.Date(paste(str_y, str_m_d, sep = "-"))
}

# Functions for plotting the regression results ================================

# Categories, that we decided not to include in the analysis, because we have
# too little data, the model is too sensitive to the assumptions etc.
get_excluded_categories <- function() {
  c(
    "API",
    "Industrial chemical",
    "PCP",
    "Pesticide"
  )
}

# Functions for the construction of confidence intervals =======================

# Function calculating the confidence intervals of the spline curve manually
# using the delta method. We can return the curve without the intercept or
# completely centered.
# Note: Removing the intercept affects the confidence intervals, centering does
# not.
calculate_spline_ci <- function(
  fitted_survreg_model,
  timeline_length,
  intercept = FALSE,
  centered = FALSE
) {
  # Prepare the time sequence of the spline. We always start from 0.
  # 'timeline_length' is the highest value of the `Date_numeric` column of our
  # data corresponding to the latest observation.
  spline_curve <- data.frame(
    Date_numeric = seq(
      from = 0,
      to = timeline_length,
      by = 1
    )
  )

  # Extract the coefficients and the variance-covariance matrix
  coeffs <- coefficients(fitted_survreg_model)
  vcov_fitted <- fitted_survreg_model$var

  # Build the P-spline basis
  basis_ps <- pspline(spline_curve$Date_numeric) |> as.matrix()

  # Locate the coefficients corresponding to the spline.
  where_ps_coeffs <- grep("ps()", names(coeffs))
  if (intercept) {
    # Add the intercept to the calculations, if required
    where_ps_coeffs <- c(1, where_ps_coeffs)
    basis_ps <- cbind(1, basis_ps)
  }
  vcov_fitted_ps <- vcov_fitted[where_ps_coeffs, where_ps_coeffs]
  coeffs_ps <- coeffs[where_ps_coeffs]

  # Calculate the fit on the response scale, possibly centered
  if (centered) {
    # We only center the fit in order to make the curve more interpretable.
    # This does not affect the standard errors, because we just graphically
    # shift the curve by a fixed distance with no uncertainty
    basis_ps_centered <- basis_ps -
      rep(1, nrow(basis_ps)) %*% t(apply(basis_ps, 2, mean))
    fit_ps <- exp(basis_ps_centered %*% coeffs_ps)
  } else {
    fit_ps <- exp(basis_ps %*% coeffs_ps)
  }

  # Calculate the standard errors on the response scale using the delta method
  se_ps <- fit_ps * sqrt(diag(basis_ps %*% vcov_fitted_ps %*% t(basis_ps)))

  spline_curve <- spline_curve |> mutate(
    fit = fit_ps,
    se = se_ps,
    lower = fit_ps - qnorm(0.975) * se_ps,
    upper = fit_ps + qnorm(0.975) * se_ps
  )
  spline_curve
}

# Function for creating the tiles displaying the age and park regression
# coefficients. It creates a data frame for plotting the tiles
extract_reg_coeffs <- function(
  covariate,
  covariate_levels,
  coeffs,
  summ
) {
  where_coeffs <- grep(covariate, names(coeffs))
  df_coeffs <- data.frame(
    # Transform the coefficients to the response scale
    Vals = exp(c(0, coeffs[where_coeffs])),
    lower = exp(
      c(
        NA,
        coeffs[where_coeffs] -
          qnorm(0.975) * summ$table[where_coeffs, "Std. Error"]
      )
    ),
    upper = exp(
      c(
        NA,
        coeffs[where_coeffs] +
          qnorm(0.975) * summ$table[where_coeffs, "Std. Error"]
      )
    ),
    p_val = c(NA, summ$table[where_coeffs, "p"]),
    coeff = factor(covariate_levels, levels = covariate_levels),
    # an x, or y value to plot the tiles in the ggplot coordinates
    dummy_value = 1,
    # Indicate, whether the tile contains a value, or is used as spacing
    empty = "non-empty"
  ) |>
    mutate(
      p_val_label = ifelse(
        p_val < 0.01,
        "p < 0.01",
        paste0("p = ", format(round(p_val, 2), nsmall = 2)
        )
      ),
      # Format the label, which will have 2 lines - the coefficient and the
      # confidence interval
      formatted_coeff_label = paste0(
        format(round(Vals, 2), nsmall = 2),
        "\n",
        # For referential categories, we do not display the confidence intervals
        ifelse(
          is.na(p_val),
          "",
          paste0(
            "(",
            format(round(lower, 2), nsmall = 2),
            ",",
            format(round(upper, 2), nsmall = 2),
            ")"
          )
        )
      ),
      # Categorize the p-value to display different levels of significance
      p_val_cat = factor(
        ifelse(
          is.na(p_val),
          "reference",
          as.character(cut(p_val, breaks = c(0, 0.01, 0.05, 0.1, 1)))
        ),
        # `get_p_val_cat_linewidth()` from the "plot_elements.R" file
        levels = names(get_p_val_cat_linewidth())
      )
    )
}

# Function for saving the data as an Excel spreadsheet =========================

save_data_as_xls <- function(dat) {
  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Extract parks
  park <- unique(dat$Park)

  # Loop through parks and add each summary to a sheet
  for (k in seq_along(park)) {
    data_chunk <- dat |> filter(Park == park[k]) |> select(-Park)
    sheet_name <- as.character(park[k])
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, data_chunk, rowNames = FALSE)
  }
  wb
}

# Functions for saving the regression results ==================================

save_results_as_xls <- function(fitted_model_list) {
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  # Loop through models and add each summary to a sheet
  for (k in seq_along(fitted_model_list)) {
    if (!is.null(fitted_model_list[[k]])) {
      model_summary <- summary(fitted_model_list[[k]])
      coef_table <- as.data.frame(model_summary$table) |>
        select(-tidyselect::any_of("z")) |>
        mutate(
          Estimate_on_the_response_scale = exp(Value),
          lwr_on_the_response_scale = exp(Value - qnorm(0.975) * `Std. Error`),
          uppr_on_the_response_scale = exp(Value + qnorm(0.975) * `Std. Error`)
        ) |>
        mutate(
          across(
            c(
              "Value",
              "Std. Error",
              "p",
              "Estimate_on_the_response_scale",
              "lwr_on_the_response_scale",
              "uppr_on_the_response_scale"
            ),
            function(x) round(x, digits = 4)
          )
        )
      sheet_name <- names(fitted_model_list)[k]
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet = sheet_name, coef_table, rowNames = TRUE)
    }
  }
  wb
}

save_results_as_csv <- function(fitted_model_list) {
  good_fitting <- fitted_model_list |>
    lapply(function(x) !is.null(x)) |>
    unlist() |>
    which()
  fitted_model_list[good_fitting] |>
    lapply(summary) |>
    lapply(function(x) x$table) |>
    lapply(as.data.frame) |>
    lapply(tibble::rownames_to_column, var = "Coefficient") |>
    bind_rows(.id = "category") |>
    mutate(
      Estimate_on_the_response_scale = exp(Value),
      lwr_on_the_response_scale = exp(Value - qnorm(0.975) * `Std. Error`),
      uppr_on_the_response_scale = exp(Value + qnorm(0.975) * `Std. Error`)
    ) |>
    mutate(
      across(
        c(
          "Value",
          "Std. Error",
          "p",
          "Estimate_on_the_response_scale",
          "lwr_on_the_response_scale",
          "uppr_on_the_response_scale"
        ),
        function(x) round(x, digits = 4)
      )
    ) |>
    select(-tidyselect::any_of("z"))
}

save_results_as_image <- function(
  plot_list,
  format = c("png", "pdf"),
  non_park_comparison = FALSE
) {
  format <- match.arg(format)

  # Loop through models and save each in a desired format
  for (k in seq_along(plot_list)) {
    if (non_park_comparison) {
      file_name <- paste0(
        "figure/Results_visualization_non_park_comparison_",
        names(plot_list)[k],
        ".",
        format
      )
    } else {
      file_name <- paste0(
        "figure/Results_visualization_",
        names(plot_list)[k],
        ".",
        format
      )
    }
    suppressWarnings(
      ggsave(
        file_name,
        plot_list[[k]],
        width = 11,
        height = 13,
        units = "in",
        dpi = 400,
        bg = "white"
      )
    )
  }
}
