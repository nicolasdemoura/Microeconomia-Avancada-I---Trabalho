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
              "foreign", "RColorBrewer")
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

###############################################################################
# Create the map
###############################################################################

RMSP <- svc("data/OD-2017/Mapas-OD2017/Shape-OD2017/Distritos_2017_region.shp")
RMSP <- vect(RMSP)

# Use RColorBrewer's "Dark1" palette for colorblind-friendly colors
colors <- brewer.pal(8, "Dark1")

# Join the data set with the map data
names(RMSP)[1] <- "Distrito"
RMSP <- RMSP[order(RMSP$Distrito), ]

# Plot the map with the Distrito variable
png("figures/distrito.png", width = 800, height = 600)
plot(RMSP, "Distrito", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
dev.off()
