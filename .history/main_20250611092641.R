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
              "writexl", "modelsummary", "estimatr", "did", "plm", "lmtest", "progress")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
gc()

# Set the random seed for reproducibility
set.seed(20250611)

####################################################################################
# Load the data
####################################################################################