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
# Clean the working environment
rm(list = ls())
load.lib <- c("dplyr", "ggplot2", "stargazer", 
              "readxl", "tidyverse", "data.table", "lubridate", "fixest", 
              "writexl", "modelsummary", "estimatr", "progress", "rsdmx",  
              "quantmod", "WDI", "forecast", "vars", "plm", "panelvar",
              "foreign", "RColorBrewer", "sf")
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
# Load the data
############################################################################### 

od_raw_data <- get_od_data()
rainfall_raw_data <- get_rainfall_data()

