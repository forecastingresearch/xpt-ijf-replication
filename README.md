# Replication package for Subjective-Probability Forecasts of Existential Risk: Initial Results from a Hybrid Persuasion-Forecasting Tournament
Package assembled on: November 7th, 2024.

Code authors: Zachary Jacobs (zach@forecastingresearch.org) and Rebecca Ceppas de Castro (rebecca@forecastingresearch.org), adapted from the [xpt-lib package](https://github.com/forecastingresearch/xpt-lib) authored by Zachary Jacobs and Molly Hickman.

## Overview & Contents
The package contains the following:
- `data/`: folder containing all raw and intermediate step data files (Summary Data/)
- `tables/`: folder containing the output files for Table 1 and Table 2 in the paper
- `figures/`: folder containing all output figures from the paper and helper scripts for their creation
- `misc/`: folder containing csv files with summary participant information
- `figures/boxplots.R`: R script for creating Figures A1, A2, and A3 in the paper
- `figures/summary-table.R`: R script to generate median forecast statistic for each group
- `figures/rs_ranks.R`: R script for generating unincentivized score ranks
- `figures/rs_quintile_plots.R`: R script for generating reciprocal score quintile plots for Stage 4
- `figures/correlation_plots.R `: R script for generating Figures A6 and A7 in the paper
- `main.R`: R script for producing all tables, figures and miscellaneous analysis in the paper
- `misc.R`: R script for summarizing participant information
- `functions.R`: R script containing helper functions for analysis/plotting
- `figures.R`: main R script for creating all figures in the paper and setting up environment for some scripts in the figures subfolder
- `tables.R`: main R script for creating all tables in the paper
## Instructions & Requirements

The analysis was run with `R (version 4.4.1)` and all the necessary packages and their versions are:
- `dplyr 1.1.4`
- `lubridade 1.9.3`
- `ggplot2 3.5.1`
- `scales 1.3.0`
- `boot 1.3.30`
- `caret 6.0.94`
- `data.table 1.15.4`
- `ggthemes 5.1.0`
- `ncar 0.5.0`
- `tidyr 1.3.1`

## Data
- All required data is found in `data/` in this repository.
- The data was collected from June 2022 to October 2022 as described in the main paper. All publicly-reported quantitative data related to the Hybrid Persuasion-Forecasting Tournament can be found in the [xpt-lib](https://github.com/forecastingresearch/xpt-lib) package repository, authored by Zachary Jacobs and Molly Hickman.
- Data underwent minimal preprocessing: data from the public survey has been cleaned to ensure all forecasts can be converted to numeric predictions. Usernames originally set on the tournament platform (e.g., `userId` in `data/forecasts.csv`) have been replaced to remove any possible identifiers.

## Additional information
- The runtime for creating all the results presented in the paper is approximately 3-10 minutes (e.g., `main.R` took roughly 3 minutes and 30 seconds to run on a 2022 M2 Macbook Air running macOS Sequoia 15.1).
