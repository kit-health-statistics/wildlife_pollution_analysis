# Pollution monitoring in German national parks

This repository contains code performing a statistical analysis of pollution levels in German national parks. We examine differences between individual parks by analyzing measurements of concentration of chemical substances, recognized as environmental pollutants, that were found in samples of deer liver.

## Data cleaning

The [data](https://github.com/barbora-sobolova/wildlife_pollution_analysis/tree/main/data) folder contains 2 batches of raw data samples from *Cervus elaphus* and *Dama dama* species:
- [`Auswertung_Winter23_24.xlsx`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/20250120_Auswertung_Winter23_24.xlsx),
- [`20250627_Auswertung_Sommer24.xlsx`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/20250627_Auswertung_Sommer24.xlsx),

An additional dataset of older samples from the *Capreolus capreolus* species is used for a complementary analysis:
- [`Daten_Wildtiere_Sachsen_ohne_PFAS.xlsx`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/Daten_Wildtiere_Sachsen_ohne_PFAS.xlsx) (Note that this set also contains samples from wild boars, which are ignored.)

The following files are present both in the CSV format and as an Excel spreadsheet. Their contents are identical.
- [`clean_data`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/clean_data.csv)
  - Produced by the [`Clean_the_data.R`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/scripts/Clean_the_data.R) script.
  - Cleaned and harmonised data from the 2 data batches in a wide format.
  - We correct mis-entered values found in the raw data file.
  - We exclude observations that should not enter the analysis.
- [`clean_roe_deer_data`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/clean_roe_deer_data.csv)
  - Produced by the [`Clean_the_roe_deer_data.R`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/scripts/Clean_the_roe_deer_data.R) script.
  - Cleaned and harmonised data from the additional dataset.
 
Note: The additional analysis using the roe deer data was sidelined. Therefore, not everything regarding the roe deer data is necessarily up to date.
 
## Data processing

- [`data_by_pollutant_category`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/data_by_pollutant_category.csv)
  - Produced by the [`Process_the_data.R`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/scripts/Process_the_data.R) script.
  - Data with values aggregated (summed) by a pollutant category.
- [`data_non_park_comparison_by_pollutant_category`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/data_non_park_comparison_by_pollutant_category.csv)
  - The additional data with values aggregated (summed) by a pollutant category.
  - Produced by the [`Process_the_data.R`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/scripts/Process_the_data.R) script.
 
The file [`chemical_categories.csv`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/chemical_categories.csv) contains the thresholds and chemical categories of individual pollutants. It is compiled from the "Stoffübersicht" sheet in [`Auswertung_Winter23_24.xlsx`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/data/20250120_Auswertung_Winter23_24.xlsx).

## The model

We use an acelerated time to failure model with explanatory variables for the **age** group, the **park** and the **day** of the sample collection. For $n$ being the number of samples, the model formula is

$$
\log(y_i) = x'\beta + \sum_{j = 1}^{12} \alpha_j s(t_i) + \sigma\varepsilon_i, \quad i = 1, \dotsc, n,
$$

where y_i denotes the response for the i-th individual, $x_i$ is the vector of the categorical covariates (age group and park), $\beta$ is a vector of its corresponding regression coefficients and $\varepsilon_i$ is the error term following the standard normal distribution. Seasonal effects are modeled using P-spline basis functions $s_j(t)$ for $j=1, \dotsc, 12$, with associated coefficients $\alpha_j$. We decided to use $12$ spline basis functions, which was a default setting of the `survreg` function.

The model is fitted using the `survreg` function from the `survival` package in `R`.

### Response $y_i$
The response is defined as a sum of observed concentrations per pollutant category. However, not all concentrations were fully quantified. Some samples are detected only qualitatively, and some were not detected. We assume that all non‑quantified values lie between 0 and the limit of quantification (LOQ). When constructing the sum $y_i$, we obtain an interval‑censored value with bounds:
- sum of quantified individual concentrations,
- sum of quantified individual concentrations + sum of the LOQs of the non-quantified, or non-detected observations.

This step is performed by `summarise_censoring` during data processing (executed by [`Process_the_data.R`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/scripts/Process_the_data.R))

### Age group
The age explanatory variable is categorical with 3 groups:
- fawn,
- subadult,
- adult.

### Park
The park explanatory variable indicates in which park a sample was collected and has 8 possible values:
- Bayerischer Wald
- Eifel
- Hainich
- Hunsrück-Hochwald
- Jasmund
- Kellerwald-Edersee
- Sächsische Schweiz
- Vorpommersche Boddenlandschaft

### Day of the sample collection
The samples of the main dataset were collected from 1. August 2023 to 31. January 2025, always during the hunting season creating a gap with no observations during the spring and early summer months. For this reason we disregard the information about the year and consider only the day of the year for the model fitting. One observation (Z91) was collected on 29. May, which again creates a gap between the bulk of the observations and this single one. As a result, we remove it before the model fitting.

For the additional dataset, observations span spring and summer as well. The timeline there spans 1. August to 9. July, and we do include observation Z91.

Discarding the year information is handled by the `unify_year` function, called during model fitting [`Fit_interval_reg.R`](https://github.com/barbora-sobolova/wildlife_pollution_analysis/blob/main/scripts/Fit_interval_reg.R).

## Reproducibility
All code was run using `R` version **4.5.0**  with the following packages
- `readxl` (1.4.5)
- `openxlsx` (4.2.8)
- `survival` (3.8-3)
- `patchwork` (1.3.0)
- `ggpubr` (0.6.0)
- `here` (1.0.1)
- `testthat` (3.2.3)
- `tidyverse` (2.0.0) bundling the following packages:
  - `dplyr` (1.1.4)
  - `forcats` (1.0.0)
  - `ggplot2` (3.5.2)
  - `lubridate` (1.9.4)
  - `purrr` (1.0.4)
  - `readr` (2.1.5)
  - `stringr` (1.5.1)
  - `tibble` (3.2.1)
  - `tidyr` (1.3.1)
