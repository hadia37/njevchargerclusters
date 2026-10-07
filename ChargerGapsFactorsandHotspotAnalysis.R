#Loading packages and data
library(tidyverse)
library(sf)
library(tigris)

chargers <- st_read("EVChargersClassified.shp")
options(tigris_use_cache = TRUE)

#Charger Accessibility
charger_coords <- st_coordinates(chargers)
