###############################################################################
# Topic: Microeconomia Avançada I
# Goal: Medir o impacto de chuva em assiduidade no trabalho
# Keywords: Labor Economics, Labor Supply, Rainfall, Climate Change
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-06-11
###############################################################################

###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("dplyr", "ipumsr", "ggplot2", "splines", "stargazer", "Hmisc", "AER",
              "readxl", "tidyverse", "data.table", "lubridate", "fixest", "pracma",
              "remotes", "tidyr", "mvProbit", "ipw", "MASS", "xtable", "quantreg",
              "nprobust", "chron", "WDI", "utils", "terra", "haven", "readr",
              "writexl", "modelsummary", "estimatr", "did", "plm", "lmtest", "progress",
              "foreign", "RColorBrewer", "sf")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
gc()

# Set the random seed for reproducibility
set.seed(20250611)

####################################################################################
# Load the data
####################################################################################

# Load the Origem e Destino (OD) data from dbf files
od_data <- read.dbf("data/OD-2017/Banco de Dados-OD2017/OD_2017_v1.dbf", as.is = TRUE)

dates <- as.Date(od_data$DATA, format = "%d%m%Y")
sort(unique(dates))
length(unique(od_data$DIA_SEM))

View(od_data[od_data$ID_DOM == sample(od_data$ID_DOM, 1), c("ID_DOM", "ID_FAM", "ID_PESS", "N_VIAG", "DATA", "DIA_SEM", "MOTIVO_O","MOTIVO_D")])

# Load the CEMADEN data

# List all CSV files in data/CEMADEN matching the pattern YYYY_MM-DD.csv
cemaden_files <- list.files("data/CEMADEN/", pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)

# Read and combine all matching files into one data frame
cemaden_data <- lapply(
    cemaden_files,
    function(file) {
        read_delim(
            file,
            delim = ";",
            col_types = cols(.default = "c")
        )
    }
) %>%
    bind_rows()

###############################################################################
# Create the map
###############################################################################

RMSP <- svc("data/OD-2017/Mapas-OD2017/Shape-OD2017/Zonas_2017_region.shp")
RMSP <- vect(RMSP)

# Use RColorBrewer's "Dark" palette for colorblind-friendly colors
colors <- brewer.pal(8, "Dark2")

# Join the data set with the map data
names(RMSP)[1] <- "Zona"
RMSP <- RMSP[order(RMSP$Zona), ]

# Plot the map with the Zona variable
png("figures/RMSP.png", width = 800, height = 600)
plot(RMSP, "Zona", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
dev.off()

# Plot the RMSP map with centroids
png("figures/RMSP_centroids.png", width = 800, height = 600)
plot(RMSP, "Zona", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
centroids <- st_centroid(st_as_sf(RMSP))
points(st_coordinates(centroids), col = "black", pch = 3, cex = 0.5)
dev.off()

#################
# Add CEMADEN stations to the map
#################

# Convert strings with commas to dots for longitude and latitude
cemaden_data <- cemaden_data %>%
  mutate(
    longitude = as.numeric(gsub(",", ".", longitude)),
    latitude = as.numeric(gsub(",", ".", latitude))
  )
  
# Don't get duplicated points
temp_data <- cemaden_data %>%
    distinct(longitude, latitude, .keep_all = TRUE)

# Create SpatVector with com CRS WGS84 (graus decimais)
temp_data <- vect(temp_data, geom = c("longitude", "latitude"), crs = "EPSG:4326")
temp_data <- project(temp_data, crs(RMSP))

# Plot the map with reprojeted points
png("figures/cemaden_points_fixed.png", width = 800, height = 600)
plot(RMSP, "Zona", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
points(temp_data, col = "red", pch = 20, cex = 0.5)
dev.off()

#################
# Voronoi polygons for CEMADEN stations
#################

# Create Voronoi polygons for CEMADEN stations
voronoi_polygons <- terra::voronoi(cemaden_vect_proj)
voronoi_polygons <- terra::project(voronoi_polygons, crs(RMSP))
terra::writeVector(voronoi_polygons, "data/OD-2017/Mapas-OD2017/Shape-OD2017/cemaden_voronoi.shp", overwrite = TRUE)

# Plot the Voronoi polygons
png("figures/cemaden_voronoi.png", width = 800, height = 600)
plot(RMSP, "Zona", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
plot(voronoi_polygons, col = "lightblue", border = "darkblue", add = TRUE, alpha = 0.5)
points(cemaden_vect_proj, col = "red", pch = 20, cex = 0.5)
dev.off()

#################
# Compute the station closest to each Zona's centroid
#################

# Extract coordinates of centroids (as matrix)
centroids_coords <- st_coordinates(centroids)

# Extract coordinates of CEMADEN stations (as matrix)
stations_coords <- st_coordinates(st_as_sf(temp_data))

# For each centroid, find the index of the closest station
closest_station_idx <- apply(
    centroids_coords, 1,
    function(centroid) {
        dists <- sqrt((stations_coords[,1] - centroid[1])^2 + (stations_coords[,2] - centroid[2])^2)
        which.min(dists)
    }
)

# Optionally, get station IDs or names if available
if ("id" %in% names(temp_data)) {
    closest_station_id <- temp_data$id[closest_station_idx]
} else {
    closest_station_id <- closest_station_idx
}

# Add closest station index (or ID) to the RMSP SpatVector
RMSP$closest_station <- closest_station_id

# Plot the RMSP map with the same color for the Zona's with the same closest station
png("figures/rmsp_closest_stations.png", width = 800, height = 600)
# Color the map based on the closest station
colors <- brewer.pal(n = length(unique(RMSP$closest_station)), name = "Set3")
plot(RMSP, "closest_station", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
points(stations_coords, col = "red", pch = 20, cex = 0.5)
points(centroids_coords, col = "black", pch = 3, cex = 0.5)
dev.off()
