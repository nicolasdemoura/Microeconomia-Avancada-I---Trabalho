###############################################################################
# Topic: Microeconomia Avançada I | Economia do Trabalho
# Goal: Functions for manipulating and analyzing data for the project
# Keywords: Labor Economics, Labor Supply, Rainfall, Absenteeism, Brazil
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-06-11
###############################################################################

###############################################################################
# Data Loading Functions
###############################################################################

get_od_data <- function() {
    data <- read.dbf("./data/OD-2017/Banco de Dados-OD2017/OD_2017_v1.dbf", as.is = TRUE)
    return(data)
}

get_rainfall_data <- function() {
    # List all CSV files in ./data/CEMADEN matching the pattern YYYY_MM-DD.csv
    cemaden_files <- list.files("./data/CEMADEN/", pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE)

    # Read and combine all matching files into one data frame
    data <- lapply(
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

    data <- data %>% rename(station_id = codEstacao)
    return(data)
}

get_georef_data <- function() {
    RMSP <- svc("./data/OD-2017/Mapas-OD2017/Shape-OD2017/Zonas_2017_region.shp")
    RMSP <- vect(RMSP)
    names(RMSP)[1] <- "Zona"
    RMSP <- RMSP[order(RMSP$Zona), ]
    return(RMSP)
}

###############################################################################
# Data Cleaning Functions
###############################################################################

clean_cemaden_data <- function(raw_cemaden_data) {
    # Convert strings with commas to dots for longitude and latitude
    temp_cemaden_data <- raw_cemaden_data %>%
        mutate(
            longitude = as.numeric(gsub(",", ".", longitude)),
            latitude = as.numeric(gsub(",", ".", latitude)),
            valorMedida = as.numeric(gsub(",", ".", valorMedida))
        )
    
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
    
    return(list(
        temp_data = temp_cemaden_data,
        daily_data = daily_cemaden_data
    ))
}

clean_od_data <- function(raw_od_data) {
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
                first(DIA_SEM) == "6" ~ "Friday",
                first(DIA_SEM) == "NA" ~ NA_character_,
                TRUE ~ "NA"
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
    
    # Create a reference date for each person based on the previous occurrence of the target day of week
    od_data <- od_data %>%
        mutate(date_ref = get_previous_dow(date, cat_weekday))
    
    return(od_data)
}

###############################################################################
# Spatial Analysis Functions
###############################################################################

create_spatial_stations <- function(temp_cemaden_data, RMSP) {
    # Create SpatVector with CRS WGS84 (graus decimais)
    stations <- temp_cemaden_data %>%
        distinct(longitude, latitude, .keep_all = TRUE)
    stations <- vect(stations, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    stations <- project(stations, crs(RMSP))
    return(stations)
}

compute_closest_stations <- function(RMSP, stations) {
    # Compute closest CEMADEN station for each Zona centroid
    centroids <- st_centroid(st_as_sf(RMSP))
    centroids_coords <- st_coordinates(centroids)
    stations_coords <- st_coordinates(st_as_sf(stations))
    closest_station <- apply(centroids_coords, 1, function(c) which.min(colSums((t(stations_coords) - c)^2)))
    RMSP$closest_station <- as.factor(if ("codEstacao" %in% names(stations)) stations$codEstacao[closest_station] else closest_station)
    return(list(RMSP = RMSP, centroids = centroids, stations_coords = stations_coords))
}

create_voronoi_polygons <- function(stations, RMSP) {
    # Create Voronoi polygons for CEMADEN stations
    voronoi_polygons <- terra::voronoi(stations)
    voronoi_polygons <- terra::project(voronoi_polygons, crs(RMSP))
    terra::writeVector(voronoi_polygons, "./data/OD-2017/Mapas-OD2017/Shape-OD2017/cemaden_voronoi.shp", overwrite = TRUE)
    return(voronoi_polygons)
}

###############################################################################
# Data Merging Functions
###############################################################################

merge_rainfall_od_data <- function(od_data, daily_cemaden_data, stations) {
    # Convert RMSP and stations to sf objects for compatibility with sf functions
    stations_sf <- st_as_sf(stations)
    
    # Precompute centroids for each Zona
    RMSP_sf <- st_as_sf(get_georef_data())
    zona_centroids <- st_centroid(RMSP_sf)
    zona_centroids$cat_zone <- as.numeric(zona_centroids$Zona)
    
    # For each date_ref, find the closest station with data for each Zona
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
    
    return(od_data)
}