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

###############################################################################
# Create spatial objects and maps
###############################################################################

# Use RColorBrewer's "Dark" palette for colorblind-friendly colors
colors <- brewer.pal(8, "Dark2")

# Create base maps
create_base_map(georef_raw_data, colors)

# Create spatial stations
stations <- create_spatial_stations(cemaden_cleaned$temp_data, georef_raw_data)

# Compute closest stations and create maps
closest_results <- compute_closest_stations(georef_raw_data, stations)
create_centroids_map(georef_raw_data, colors, closest_results$centroids)
create_stations_map(georef_raw_data, colors, stations)
create_closest_stations_map(closest_results$RMSP, closest_results$stations_coords, st_coordinates(closest_results$centroids))

# Create Voronoi polygons and map
voronoi_polygons <- create_voronoi_polygons(stations, georef_raw_data)
create_voronoi_map(closest_results$RMSP, voronoi_polygons, stations, st_coordinates(closest_results$centroids))

###############################################################################
# Merge data and create visualizations
###############################################################################

# Merge rainfall and OD data
merged_data <- merge_rainfall_od_data(od_cleaned, cemaden_cleaned$daily_data, stations)

# Create histograms
create_rainfall_histogram(cemaden_cleaned$daily_data)
create_trips_histogram(merged_data)

