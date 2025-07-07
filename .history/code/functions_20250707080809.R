###############################################################################
# Topic: Microeconomia Avançada I | Economia do Trabalho
# Goal: Functions for manipulating and analyzing data for the project
# Keywords: Labor Economics, Labor Supply, Rainfall, Absenteeism, Brazil
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-06-11
###############################################################################

###############################################################################
# Load the OD data
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
}