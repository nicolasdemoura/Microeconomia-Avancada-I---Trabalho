###############################################################################
# Topic: Microeconomia Avançada I | Economia do Trabalho
# Goal: Rainfall effect on absenteeism in São Paulo, Brazil 
# Keywords: Labor Economics, Labor Supply, Rainfall, Absenteeism, Brazil
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-06-11
###############################################################################

###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("dplyr", "ggplot2", "stargazer", 
              "readxl", "tidyverse", "data.table", "lubridate", "fixest", 
              "writexl", "modelsummary", "estimatr", "progress", "rsdmx",  
              "quantmod", "WDI", "forecast", "vars", "plm", "panelvar",
              "foreign", "RColorBrewer", "sf", "terra", "readr", "purrr")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
rm(install.lib, lib, load.lib)
gc()

# Set the random seed for reproducibility
set.seed(20250611)

# Import functions
source("code/functions.R")
source("code/plotting.R")

###############################################################################
# Load and clean the data
############################################################################### 

# Load raw data
od_raw_data <- get_od_data()
rainfall_raw_data <- get_rainfall_data()
georef_raw_data <- get_georef_data()

# Clean data
cemaden_cleaned <- clean_cemaden_data(rainfall_raw_data)
od_cleaned <- clean_od_data(od_raw_data)

# Create spatial stations
stations <- create_spatial_stations(cemaden_cleaned$temp_data, georef_raw_data)

###############################################################################
# Merge data and create visualizations
###############################################################################

# Merge rainfall and OD data
merged_data <- merge_rainfall_od_data(od_cleaned, cemaden_cleaned$daily_data, stations)

###############################################################################
# Create visualizations
###############################################################################

# Create density plots for rainfall and trips
density_plots <- create_rainfall_trips_density(analysis_data)

# Create combined density plot
combined_density <- create_combined_density(analysis_data)

# Create rainfall histogram using style
rainfall_hist <- gg_histogram(
    plot_data = analysis_data,
    x_var = "total_rain",
    title = "Daily Rainfall Distribution",
    subtitle = "Frequency distribution of rainfall measurements across São Paulo",
    x_label = "Total Rainfall (mm)",
    y_label = "Frequency",
    filename = "figures/rainfall_histogram.png",
    bar_color = "#CC0000",
    bins = 50
)

# Create trips histogram using style
trips_hist <- gg_histogram(
    plot_data = analysis_data,
    x_var = "num_trips",
    title = "Daily Trips Distribution",
    subtitle = "Frequency distribution of daily trips per person",
    x_label = "Number of Trips",
    y_label = "Frequency",
    filename = "figures/trips_histogram.png",
    bar_color = "#46647B",
    bins = 20
)