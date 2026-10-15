#Loading packages and data
library(tidyverse)
library(sf)
library(tigris)

#Rural municipality charger gaps: no chargers within 10 miles of 3mi buffer of major corridors and 3mi radius of municipality greater than 10,000
library(nngeo)
majorroutes <- st_read("Major_Routes.shp")
ruralmunis <- st_read("Rural Municipalities.shp")
chargers <- st_read("EVChargersClassified.shp")

#reproject for distance calculations
majorroutes <- st_transform(majorroutes, 3857)
ruralmunis <- st_transform(ruralmunis, 3857)
chargers <- st_transform(chargers, 3857)
dcfc <- chargers%>%
  filter(level == "DCFC") #still includes urban chargers 
#define parameters and create buffers (try different distances)
corridor_buffer_miles <- 3
gap_distance_miles <- 10

mi_to_m <- 1609.344 #conversion factor
corridor_buf_m <- corridor_buffer_miles * mi_to_m
gap_dist_m <- gap_distance_miles * mi_to_m

corridor_buffer <- st_buffer(majorroutes, corridor_buf_m)
  #st_union(corridor_buffer)

#Calculate distances from each municipality to nearest DCFC and add to municipality dataset
nearest <- st_nn(ruralmunis, dcfc, k = 1, returnDist = TRUE)
ruralmunis$dist_to_dcfc_m <- unlist(nearest$dist)

##Rule 1: Corridor-based gaps. Will assign TRUE if meets gap parameters, FALSE if not
corridor_munis <- st_intersects(ruralmunis, corridor_buffer, sparse = FALSE) #creates matrix for each muni by each corridor
ruralmunis$touches_corridor <- rowSums(corridor_munis) > 0
ruralmunis$corridor_gap <-
  ruralmunis$touches_corridor &
  (ruralmunis$dist_to_dcfc_m > gap_dist_m)

##Rule 2: Population-based gaps
ruralmunis$pop_gap <-
(ruralmunis$population > 10000) &
(ruralmunis$dist_to_dcfc_m > gap_dist_m)

#Combine gaps
ruralmunis$gap_flag <- ruralmunis$corridor_gap | ruralmunis$pop_gap

#HOTSPOT ANALYSIS for urban areas?

#Accessibility Analysis
library(osrm) 
library(dodgr) 
library(stplanr) 
library(spdep) 
library(sfdep) 
library(osmextract)

chargers <- st_read("EVChargersClassified.shp")
options(tigris_use_cache = TRUE)

#Starting with calculating nearest travel distance to gas stations in rural vs urban areas to use as threshold
gas <- st_read("Gas_Service_Stations_of_New_Jersey.shp")
gas <- st_transform(gas, 4326)
urban_rural <- st_read("UrbanRural.shp")
gas <- st_join(gas, urban_rural)

#Create travel distance matrix for gas stations
nj_osm <- oe_get("New Jersey", layer = "lines")
graph <- weight_streetnet(nj_osm)
if (file.exists("nj_graph.rds")) {
  graph <- readRDS("nj_graph.rds")
} else {
  nj_osm <- oe_get_network(
    place = "New Jersey",
    mode = "driving"
  )
  graph <- weight_streetnet(nj_osm)
  saveRDS(graph, "nj_graph.rds")
}

gas_df <- gas %>%
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>%
  st_drop_geometry()

dist_matrix <- dodgr=dists(graph, from = gas_df, to = gas_df)
gas$nn_dist_m <- apply(dist_matrix + diag(Inf, nrow(dist_matrix)), 1, min)

#tm <- osrmTable(src = gas_df[, c("lon", "lat")], dst = gas_df[, c("lon", "lat")])
#gas$nn_time_min <- apply(tm$durations + diag(Inf, nrow(tm$durations)), 1, min)

#Calculating threshold travel distance for rural vs urban using mean/median
#Should i use standard deviation or percentiles instead? Switching to dodgr instead of osrm
urban_gas <- gas %>%
  fiter(urban_rural == "urban")
rural_gas <- gas %>%
  fiter(urban_rural == "rural")
urban_dist_threshold <- median(urban_gas$nn_dist_m, na.rm = TRUE)
rural_dist_threshold <- median(rural_gas$nn_dist_m, na.rm = TRUE)

# Now for chargers: Create travel distance matrix
charger_coords <- st_coordinates(chargers) 
chargers_df <- chargers %>% 
  mutate(lon = charger_coords[,1], 
         lat = charger_coords[,2]) %>% 
  st_drop_geometry() 

dist_matrix2 <- dodgr=dists(graph, from = chargers_df, to = chargers_df)
chargers$nn_dist_m <- apply(dist_matrix2 + diag(Inf, nrow(dist_matrix2)), 1, min)

#travelmatrix <- osrmTable(src = chargers_df[, c("lon", "lat")], 
                #dst = chargers_df[, c("lon", "lat")]) 
#chargers$nearest_dist_min <- apply(travelmatrix$durations, 1, min) 

#Classify gap areas
chargers <- chargers %>% 
  mutate( 
    gap_flag = case_when( 
      urban_rural == "rural" & nn_dist_m > rural_dist_threshold ~ TRUE, 
      urban_rural == "urban" & nn_dist_m > urban_dist_threshold ~ TRUE, 
      TRUE ~ FALSE 
    ) 
  ) 