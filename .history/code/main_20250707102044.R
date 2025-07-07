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
load.lib <- c("dplyr", "ggplot2", "stargazer", "sandwich", "fixest",
              "foreign", "RColorBrewer", "sf", "terra", "readr", "purrr")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
rm(install.lib, lib, load.lib)
gc()

# Set the random seed for reproducibility
set.seed(20250611)

# Import functions
source("code/data.R")
source("code/plotting.R")
source("code/analysis.R")

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

# Merge rainfall and OD data
merged_data <- merge_rainfall_od_data(od_cleaned, cemaden_cleaned$daily_data, stations)

###############################################################################
# Create visualizations
###############################################################################


#########
# Create descriptive statistics 
#########


#########
# Create histogram visualizations
#########

# Create rainfall histogram
rainfall_hist <- gg_histogram(
    plot_data = merged_data,
    x_var = "total_rain",
    title = "", #"Daily Rainfall Distribution",
    subtitle = "", #"Frequency distribution of rainfall measurements across São Paulo",
    x_label = "Total Rainfall (mm)",
    y_label = "Frequency",
    filename = "figures/rainfall_histogram.png",
    bar_color = "#CC0000",
    bins = 50
)

# Create trips histogram
trips_hist <- gg_histogram(
    plot_data = merged_data,
    x_var = "num_trips",
    title = "", #"Daily Trips Distribution",
    subtitle = "", #"Frequency distribution of daily trips per person",
    x_label = "Number of Trips",
    y_label = "Frequency",
    filename = "figures/trips_histogram.png",
    bar_color = "#46647B",
    bins = 20
)

# Create duration histogram
duration_hist <- gg_histogram(
    plot_data = merged_data[merged_data$dm_worked == 1,],
    x_var = "num_duration",
    title = "", #"Commute Duration Distribution",
    subtitle = "", #"Frequency distribution of daily trip durations",
    y_label = "Frequency",
    filename = "figures/duration_histogram.png",
    x_label = "Duration (minutes)",
    bar_color = "#FFB300",
)

#########
# Create spatial visualizations
#########

# Compute closest stations and create maps
closest_results <- compute_closest_stations(georef_raw_data, stations)

# Create Voronoi polygons
voronoi_polygons <- create_voronoi_polygons(stations, georef_raw_data)

# Create base RMSP map
rmsp_base <- gg_rmsp_base(
    RMSP = georef_raw_data,
    title = "São Paulo Metropolitan Region",
    subtitle = "Base map showing administrative zones",
    filename = "figures/rmsp_base.png"
)

# Create RMSP map with Voronoi polygons
rmsp_voronoi <- gg_rmsp_voronoi(
    RMSP = georef_raw_data,
    stations = stations,
    voronoi_polygons = voronoi_polygons,
    title = "RMSP with CEMADEN Weather Stations",
    subtitle = "Voronoi polygons showing coverage areas for each weather station",
    filename = "figures/rmsp_voronoi.png",
    show_stations = TRUE,
    show_centroids = TRUE,
    show_voronoi = TRUE,
    show_rmsp_fill = TRUE
)

###############################################################################
# Run regression analysis
###############################################################################

# Run main regression analysis
analysis_results <- run_main_analysis(merged_data)

# Create and export regression table
regression_table <- export_regression_table(analysis_results, "output/regression_results.tex")
