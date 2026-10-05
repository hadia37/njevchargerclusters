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
nj <- nj |>
  st_union() |> 
  st_make_valid() |>
  st_buffer(0)
#fixes multipolygon issue^
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
  geom_sf(data = urban_union, aes(fill = "Urban"), color = NA) +
  scale_fill_manual(
    values = c("Urban" = "grey60", "Rural" = "lightgreen"),
    name = "Area Type"
  ) + 
  labs(title = "New Jersey Urban and Rural Areas") +
  theme_minimal()
ggsave("New Jersey Urban and Rural Areas.png")

#Classify chargers and link to rural/urban areas
ev_stations <- ev_stations |>
  filter(EV_Level2_EVSE_Num > 0 | EV_DC_Fast_Count > 0) |>
  mutate(
    level = case_when(
      !is.na(EV_DC_Fast_Count) & EV_DC_Fast_Count > 0 ~ "DCFC",
      TRUE ~ "L2"
    )
  )
# Convert the geometry objects explicitly to sf dataframes with a new label column
urban_nj_clean <- st_sf(urban_rural = "urban", geometry = st_geometry(urban_union))
rural_nj_clean <- st_sf(urban_rural = "rural", geometry = st_geometry(rural_nj))
# Combine them cleanly
urban_rural <- bind_rows(urban_nj_clean, rural_nj_clean)
urban_rural <- st_transform(urban_rural, 4326)
urban_rural <- urban_rural |>
  st_make_valid() |>
  st_collection_extract("POLYGON") |>
  st_buffer(0)

#Convert charger dataset to sf before spatial join
chargers_sf <- st_as_sf(ev_stations, coords = c("Longitude", "Latitude"), crs=4326)
chargers <- st_join(chargers_sf, urban_rural)
st_write(chargers, "EVChargersClassified.shp")
