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

#################
# Load the Origem e Destino (OD) data from dbf files
#################

raw_od_data <- read.dbf("data/OD-2017/Banco de Dados-OD2017/OD_2017_v1.dbf", as.is = TRUE)

#################
# Load the CEMADEN data
#################

# List all CSV files in data/CEMADEN matching the pattern YYYY_MM-DD.csv
cemaden_files <- list.files("data/CEMADEN/", pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)

# Read and combine all matching files into one data frame
raw_cemaden_data <- lapply(
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

raw_cemaden_data <- raw_cemaden_data %>% rename(station_id = codEstacao)

###############################################################################
# Create maps
###############################################################################

#################
# Load the map of the RMSP (Região Metropolitana de São Paulo)
#################

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
temp_cemaden_data <- raw_cemaden_data %>%
    mutate(
        longitude = as.numeric(gsub(",", ".", longitude)),
        latitude = as.numeric(gsub(",", ".", latitude)),
        valorMedida = as.numeric(gsub(",", ".", valorMedida))
    )

# Create SpatVector with CRS WGS84 (graus decimais)
stations <- temp_cemaden_data %>%
        distinct(longitude, latitude, .keep_all = TRUE)
stations <- vect(stations, geom = c("longitude", "latitude"), crs = "EPSG:4326")
stations <- project(stations, crs(RMSP))

# Plot the map with reprojected points
png("figures/RMSP_cemaden_stations.png", width = 800, height = 600)
plot(RMSP, "Zona", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
points(stations, col = "red", pch = 20, cex = 0.5)
dev.off()

#################
# Compute the station closest to each Zona's centroid
#################

# Compute closest CEMADEN station for each Zona centroid
centroids_coords <- st_coordinates(centroids)
stations_coords <- st_coordinates(st_as_sf(stations))
closest_station <- apply(centroids_coords, 1, function(c) which.min(colSums((t(stations_coords) - c)^2)))
RMSP$closest_station <- as.factor(if ("codEstacao" %in% names(stations)) stations$codEstacao[closest_station] else closest_station)

# Plot
n_colors <- length(levels(RMSP$closest_station))
colors_closest <- colorRampPalette(brewer.pal(12, "Set3"))(n_colors)
png("figures/RMSP_closest_stations.png", width = 800, height = 600)
plot(RMSP, "closest_station", col = colors_closest, axes = FALSE, legend = TRUE, border = "#FFFFFF")
points(stations_coords, col = "red", pch = 20, cex = 0.5)
points(centroids_coords, col = "black", pch = 3, cex = 0.7)
dev.off()

#################
# Voronoi polygons for CEMADEN stations
#################

# Create Voronoi polygons for CEMADEN stations
voronoi_polygons <- terra::voronoi(stations)
voronoi_polygons <- terra::project(voronoi_polygons, crs(RMSP))
terra::writeVector(voronoi_polygons, "data/OD-2017/Mapas-OD2017/Shape-OD2017/cemaden_voronoi.shp", overwrite = TRUE)

# Plot the Voronoi polygons (no fill, only borders), add centroids
png("figures/RMSP_voronoi.png", width = 800, height = 600)
plot(RMSP, "Zona", col = colors_closest, axes = FALSE, legend = TRUE, border = "#FFFFFF")
plot(voronoi_polygons, border = "darkblue", add = TRUE, lwd = 1)
points(stations, col = "red", pch = 20, cex = 0.5)
points(centroids_coords, col = "black", pch = 3, cex = 0.7)
dev.off()

###############################################################################
# Cleaning the data
###############################################################################

#################
# Clean the CEMADEN data 
#################

# Get the first 10 characters of the date column to create a day variable
temp_cemaden_data$date_ref <- substr(temp_cemaden_data$datahora, 1, 10)
temp_cemaden_data$date_ref <- as.Date(temp_cemaden_data$date_ref, format = "%Y-%m-%d")

# Group by day and station, and calculate the sum of the valorMedida
daily_cemaden_data <- temp_cemaden_data %>%
    group_by(date_ref, station_id) %>%
    summarise(
        station_id = first(station_id),
        date_ref = first(date_ref),
        total_rain = sum(valorMedida, na.rm = TRUE),
        .groups = "drop"
    )

# Plot histogram of total rain
gg <- ggplot(daily_cemaden_data, aes(x = total_rain)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = "Histogram of Total Rainfall", x = "Total Rainfall (mm)", y = "Frequency") +
    theme_minimal()
ggsave("figures/histogram_total_rainfall.png", plot = gg, width = 8, height = 6)

#################
# Clean the OD data
#################

# Select relevant columns and rename them for clarity
od_data <- raw_od_data %>%
    group_by(ID_PESS, DATA) %>%
    summarise(
        id_person = as.numeric(first(ID_PESS)),
        date = as.Date(paste0(substr(first(DATA), 5, 8), "-", substr(first(DATA), 3, 4), "-", substr(first(DATA), 1, 2)), format = "%Y-%m-%d"),
        cat_weekday = case_when(
            first(DIA_SEM) == "2" ~ "Monday",
            first(DIA_SEM) == "3" ~ "Tuesday",
            first(DIA_SEM) == "4" ~ "Wednesday",
            first(DIA_SEM) == "5" ~ "Thursday",
            first(DIA_SEM) == "NA" ~ NA_character_,
            TRUE ~ "Friday"
        ),
        num_age = first(IDADE),
        dm_male = ifelse(first(SEXO) == "1", 1, 0),
        cat_educ = first(GRAU_INS),
        cat_actv = first(CD_ATIVI),
        cat_incm = first(CO_REN_I),
        cat_zone = as.numeric(first(ZONA)),
        cat_zone_o = as.numeric(first(ZONA_O)),
        cat_zone_d = as.numeric(first(ZONA_D)),
        cat_zone_emp = first(ZONATRA1),
        cat_ocup = first(OCUP1),
        cat_setr = first(SETOR1),
        cat_vinc = first(VINC1),
        num_income = first(VL_REN_I),
        num_trips = first(TOT_VIAG),
        dm_worked = max(ifelse(MOTIVO_O %in% c(1, 2, 3), 1, 0), na.rm = TRUE),
        num_duration = first(DURACAO)
    ) %>%
    ungroup()

# Function to get the previous date matching the target day of week
get_previous_dow <- function(date, dow_target) {
  # Convert day name to number (1 = Sunday, 2 = Monday, ..., 7 = Saturday)
  target_num <- match(dow_target, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Get day number of date
  current_num <- wday(date)
  
  # Days to subtract
  days_back <- (current_num - target_num) %% 7
  days_back[days_back == 0] <- 7  # Ensure we go to the *previous* occurrence

  date - days_back
}

# Create a reference date for each person based on the previous occurrence of the target day of week
od_data <- od_data  %>%
  mutate(date_ref = get_previous_dow(date, cat_weekday)) 

#################
# Merge CEMADEN data with OD data
#################

# Convert RMSP and stations to sf objects for compatibility with sf functions
RMSP_sf <- st_as_sf(RMSP)
stations_sf <- st_as_sf(stations)

# Precompute centroids for each Zona
zona_centroids <- st_centroid(RMSP_sf)
zona_centroids$cat_zone <- as.numeric(zona_centroids$Zona)

# For each date_ref, find the closest station with data for each Zona
library(purrr)
date_station_map <- daily_cemaden_data %>%
  dplyr::select(date_ref, station_id) %>%
  distinct() %>%
  group_by(date_ref) %>%
  summarise(stations = list(station_id), .groups = "drop")

# Function to get closest station for all zonas for a given date_ref
get_closest_stations_for_date <- function(date_ref, stations_ids) {
  # Filter stations_sf for available stations
  available_stations <- stations_sf %>% filter(station_id %in% stations_ids)
  # Compute distances from each centroid to all available stations
  dists <- st_distance(zona_centroids, available_stations)
  # For each centroid, get the closest station
  closest_idx <- apply(dists, 1, which.min)
  tibble(
    date_ref = date_ref,
    cat_zone = zona_centroids$cat_zone,
    closest_station = available_stations$station_id[closest_idx]
  )
}

# Build mapping for all date_ref/zona combinations
closest_station_map <- map2_dfr(date_station_map$date_ref, date_station_map$stations, get_closest_stations_for_date)

# Merge OD data with closest station mapping
od_data <- od_data %>%
  left_join(closest_station_map, by = c("date_ref", "cat_zone"))

# Merge OD data with daily rainfall by closest station and date
od_data <- od_data %>%
    left_join(
        daily_cemaden_data %>%
            dplyr::select(date_ref, station_id, total_rain) %>%
            rename(date_ref = date_ref, closest_station = station_id),
        by = c("date_ref", "closest_station")
    )

# Drop rows with NA and convert to numeric
od_data <- od_data %>%
    filter(!is.na(total_rain)) %>%
    mutate(total_rain = as.numeric(total_rain))

###############################################################################
# Run a simple regression model of rainfall on trips
###############################################################################

# Add dummy variables for rainfall and employment status
data <- od_data %>%
    mutate(
        dm_employed = ifelse(cat_actv %in% c("1", "2", "3"), 1, 0),
        dm_rain = ifelse(total_rain > 0, 1, 0))

##############
# Effects on num_trips
##############

hist(od_data$num_trips, breaks = 50, main = "Histogram of Number of Trips", xlab = "Number of Trips", col = "lightblue")

reg_model <- lm(num_trips ~ total_rain, data = data)
reg_model_dummy <- lm(num_trips ~ dm_rain, data = data)
reg_model_fe <- feols(num_trips ~ dm_rain | cat_zone, data = od_data)
summary(reg_model)
summary(reg_model_dummy)
summary(reg_model_fe)

##############
# Effects on dm_worked
##############

hist(data$total_rain, breaks = 100, main = "Histogram of Total Rainfall", xlab = "Total Rainfall (mm)", col = "lightblue")

reg_model_worked <- feols(dm_worked ~ dm_rain + total_rain | cat_zone, data = data[data$dm_employed == 1, ])
summary(reg_model_worked)
