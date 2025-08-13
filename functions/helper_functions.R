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
    x %in% c("männlich", "männlich\r\n") ~ "Male",
    x %in% c("weiblich", "weiblich\r\n") ~ "Female"
  )
}

# Relabeling function for the age categories. Input is the character
# vector of the age category names.
relabel_age <- function(x) {
  case_when(
    x == "adult" ~ "Adult",
    x == "subadult" ~ "Subadult",
    x == "Kalb" ~ "Calf"
  )
}

# Sorting samples into seasons by the sample date. The cut-off points are
# orientational based on which dataset corresponds to which season. Input is the
# character date of the sample collection vector.
season_from_date <- function(x) {
  case_when(
    x < as.Date("2024-05-01") ~ "Winter 2023/24",
    x > as.Date("2024-10-02") ~ "Winter 2024/25",
    x <= as.Date("2024-10-02") & x >= as.Date("2024-05-01") ~ "Summer 2024"
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
      age == 0.5 ~ "Calf",
      age > 0.5 & age <= 1.5 ~ "Subadult",
      age > 1.5 ~ "Adult"
    )
  } else {
    case_when(
      age %in% c("Altersklasse 0", "Altersklasse 0\r\n") ~ "Calf",
      age %in% c("Altersklasse 1", "Altersklasse 1\r\n") ~ "Subadult",
      age %in% c("Altersklasse 2", "Altersklasse 2\r\n") ~ "Adult"
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

# Function determining, whether a pollutant category was quantified, detected,
# or not detected. Input is a character vector of detection indicators for
# individual chemical.
summarise_detection <- function(x) {
  if (all(x == "Not detected")) {
    # Category was not detected, when no chemicals from the category were
    # detected
    ret <- "Not detected"
  } else if (any(x == "Quantified")) {
    # Category was quantified, when at least one chemical from the category was
    # quantified
    ret <- "Quantified"
  } else {
    # Category was detected, when at least one chemicals from the category was
    # qualitatively detected and no chemicals from the category were quantified
    ret <- "Detected"
  }
  ret
}

# This function creates a range per a chemical category, where the sum of all
# observed values lie. This accounts for the censoring when a chemical is
# detected only qualitatively.
# IMPORTANT: When a chemical was not detected, we consider these observations as
# zeros, but set them to a small value close to zero, e.g. 1e-4 in order to be
# able to fit a lognormal model. When a chemical was detected only
# qualitatively, we assume these to lie anywhere between 0 and their detection
# threshold.
summarise_censoring <- function(detected, value, threshold) {
  vals_sum <- sum(value, na.rm = TRUE)
  if (all(detected == "Not detected")) {
    # The aggregated minimum value is set as (near) zero, the max value is the
    # sum of LOQs (the worst scenario)
    ret <- c(
      censored = "Non-detect",
      Value_min = 1e-6,
      # Treat the non-detects just like the non-quantified values, because we
      # do not have the limits of detection. As it stands, the model is not able
      # to estimate some coefficients for the Industrial chemical category.
      Value_max = sum(threshold)
    )
  } else if (
    all(detected == "Quantified" | detected == "Not detected") &&
      !all(detected == "Not detected")
  ) {
    # The aggregated value is a sum of the quantified values with no
    # uncertainty. Not detected values are treated as zeros.
    ret <- c(
      censored = "Fully quantified",
      Value_min = vals_sum,
      Value_max = vals_sum
    )
  } else if (any(detected == "Detected")) {
    # The aggregated value of the detected part lies between 0 and the sum of
    # thresholds. Then the quantified part is added.
    ret <- c(
      censored = "Left censored",
      Value_min = ifelse(vals_sum == 0, 1e-6, vals_sum),
      Value_max = vals_sum + sum(threshold[detected == "Detected"])
    )
  }
  ret
}

# Function for removing the information on year from a date by unifying it to
# an (almost) arbitrary year 2022/2023. The only non-arbitrary thing is that
# 2023 was not a leap year. We will have to think about it again, if we ever
# get samples from 29.02.
unify_year <- function(date_vec) {
  str_m_d <- paste(
    stringr::str_pad(month(date_vec), pad = "0", side = "left", width = 2),
    stringr::str_pad(day(date_vec), pad = "0", side = "left", width = 2),
    sep = "-"
  )
  # Cut off at the 3th month, so that we have the beginning in spring and the
  # end in winter for both the deer and roe deer data.
  str_y <- ifelse(month(date_vec) < 3, "2023", "2022")

  as.Date(paste(str_y, str_m_d, sep = "-"))
}

# Functions for plotting the regression results ================================

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
    p_val = c(NA, summ$table[where_coeffs, "p"]),
    coeff = factor(covariate_levels, levels = covariate_levels),
    # an x, or y value to plot the tiles in the ggplot coordinates
    dummy_value = 1,
    # Indicate, whether the tile contains a value, or is used as spacing
    empty = "non-empty"
  ) |>
    mutate(
      formatted_label = paste0(
        format(round(Vals, 2), nsmall = 2),
        "\np = ",
        format(round(p_val, 2), nsmall = 2)
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

# Functions for saving the regression results ==================================

save_results_as_xls <- function(fitted_model_list) {
  # Create a new workbook
  wb <- openxlsx::createWorkbook()
  # Loop through models and add each summary to a sheet
  for (k in seq_along(fitted_model_list)) {
    model_summary <- summary(fitted_model_list[[k]])
    coef_table <- as.data.frame(model_summary$table) |>
      select(-tidyselect::any_of("z")) |>
      mutate(
        across(c("Value", "Std. Error", "p"), function(x) round(x, digits = 4))
      )
    sheet_name <- names(fitted_model_list)[k]
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(wb, sheet = sheet_name, coef_table, rowNames = TRUE)
  }
  wb
}

save_results_as_csv <- function(fitted_model_list) {
  fitted_model_list |>
    lapply(summary) |>
    lapply(function(x) x$table) |>
    lapply(as.data.frame) |>
    lapply(tibble::rownames_to_column, var = "Coefficient") |>
    bind_rows(.id = "category") |>
    mutate(
      across(c("Value", "Std. Error", "p"), function(x) round(x, digits = 4))
    ) |>
    select(-tidyselect::any_of("z"))
}

save_results_as_image <- function(plot_list, non_park_comparison = FALSE) {
  # Loop through models and add each summary to a sheet
  for (k in seq_along(plot_list)) {
    if (non_park_comparison) {
      file_name <- paste0(
        "figure/Results_visualization_non_park_comparison_",
        names(plot_list)[k],
        ".pdf"
      )
    } else {
      file_name <- paste0(
        "figure/Results_visualization_",
        names(plot_list)[k],
        ".pdf"
      )
    }
    ggsave(
      file_name,
      plot_list[[k]],
      width = 10,
      height = 9
    )
  }
}