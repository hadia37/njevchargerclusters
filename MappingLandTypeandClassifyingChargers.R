#Loading packages
library(tidyverse) #includes ggplot2
library(sf)
library(tigris) #for NJ boundaries

#Loading data
ev_stations <- read.csv("alt_fuel_stations (Aug 26 2026).csv") #can get a more updated version for final analysis
census_urban_areas <- st_read("USA_Census_Urban_Areas.shp")

#Get the official New Jersey county boundary outlines to clip urban areas
options(tigris_use_cache = TRUE) #accesses already downloaded boundaries 
nj <- states(cb = TRUE) |>
  filter(STUSPS == "NJ") |>
  st_transform(st_crs(census_urban_areas))
nj <-
  st_union(nj) |> #to fix multipolygon issue
  st_make_valid(nj) |>
  st_buffer(0)
urban_nj <- st_intersection(census_urban_areas, nj)|>
  st_make_valid() |>
  st_buffer(0)
  #st_transform(st_crs(32111)) reprojecting all data to NJ State Plane

#Create rural areas = NJ - urban areas
urban_union <- st_union(urban_nj) |> #merging urban multipolygon into one polygon
  st_make_valid() |>
  st_buffer(0)
rural_nj <- st_difference(nj, urban_union)

#Plot urban and rural areas map
ggplot()+
  geom_sf(data = nj, aes(fill = NA), color = "black") +
  geom_sf(data = rural_nj, aes(fill = "Rural"), color = NA) +
  geom_sf(data = urban_nj, aes(fill = "Urban"), color = NA) +
  scale_fill_manual(
    values = c("Urban" = "grey60", "Rural" = "lightgreen"),
    name = "Area Type"
  ) + 
  labs(title = "New Jersey Urban and Rural Areas") +
  theme_minimal()
  
#or
plot(st_geometry(nj), col = "white", border = "black")
plot(st_geometry(rural_nj), col = "lightgreen", add = TRUE)
plot(st_geometry(urban_nj), col = "grey60", add = TRUE)
title("New Jersey Urban vs Rural Areas")
legend(
  "bottomright",
  legend = c("Urban", "Rural")
  fill = c("grey60", "lightgreen")
  border = "black"
  bg = "white"
  cex = 0.8
)

#Convert charger dataset to sf and reproject
chargers_sf <- st_as_sf(ev_stations, coords = c("Longitude", "Latitude"), crs=4326)
charger_reproj <- st_transform(chargers_sf, 3857)
coords <- st_coordinates(charger_reproj)


