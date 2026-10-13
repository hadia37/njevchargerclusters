#Loading packages and data
library(tidyverse)
library(sf)
library(tigris)

library(osrm) 
library(dodgr) 
library(stplanr) 
library(spdep) 
library(sfdep) 
library(osmextract)

chargers <- st_read("EVChargersClassified.shp")
options(tigris_use_cache = TRUE)

#Accessibility Analysis
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