#Loading packages and data
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE) 

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
gap_distance_miles <- 15

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
(ruralmunis$POP2020 > 5000) &
(ruralmunis$dist_to_dcfc_m > gap_dist_m)

#Combine gaps
ruralmunis$gap_flag <- ruralmunis$corridor_gap | ruralmunis$pop_gap
ruralmunis$gap_type <- case_when(
  ruralmunis$corridor_gap & ruralmunis$pop_gap ~ "Both",
  ruralmunis$pop_gap ~ "Population Gap",
  ruralmunis$corridor_gap ~ "Corridor Gap",
  TRUE ~ "No Gap"
)
write_sf(ruralmunis, "Rural Municipality Gaps.shp")
ruralmuni_df <-
  st_drop_geometry(ruralmunis)
write_csv(ruralmuni_df, "Rural Municipality Gaps.csv")

#Visualization
nj <- states(cb = TRUE) |>
  filter(STUSPS == "NJ")
nj <- st_transform(nj, st_crs(ruralmunis))
ggplot() + 
  geom_sf(data = nj, fill = "white", color = "black", linewidth = 0.5) + 
  geom_sf(data = ruralmunis, aes(fill = gap_type), color = "grey30", size = 0.2) + 
  scale_fill_manual(
    values = c(
      "Corridor Gap" = "lightblue", 
      "Population Gap" = "pink", 
      "Both" = "lavender", 
      "No Gap" = "grey80"
    ),
    breaks = c(
      "Corridor Gap",
      "Population Gap",
      "Both",
      "No Gap"
    ),
    drop = FALSE,
    na.value = "red"
  ) + 
  theme_minimal() + 
  labs(
    title = "Rural Municipality DCFC Gaps", 
    fill = "Gap Type"
  )
ggsave("Rural Municipality DCFC Gaps.png")


#HOTSPOT ANALYSIS for urban areas
library(spdep) 
#Count chargers by municipality for DCFC
urbanmunis <- st_read("Urban Municipalities.shp")
urbanmunis <- st_transform(urbanmunis, 3857)
urbanmunis <- urbanmunis %>% 
  mutate(n_dcfc = 
           lengths(st_intersects(urbanmunis, dcfc)))
urbanmunis$n_dcfc <- as.numeric(urbanmunis$n_dcfc)

#preliminary visualization of charger counts
library(scales)
library(RColorBrewer)
my_breaks <- c(0, 1, 2, 3, 4, 5, 10, 15)
my_colors <- brewer.pal(7, "YlOrRd") 
ggplot()+
  geom_sf(data = nj, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(data = urbanmunis, aes(fill= n_dcfc), color = "black", linewidth = 0.15) +
  scale_fill_gradientn(
    name = "DCFC Charger Count", 
    colors = my_colors,
    values = rescale(my_breaks, to = c(0, 1), from = c(0, 15)),
    breaks = c(0, 1, 2, 3, 4, 5, 10, 15),
    #low = "white", 
    #mid = "lavender", 
    #high = "purple", 
    #midpoint = mean(urbanmunis$charger_count, na.rm = TRUE)
    ) +
  theme_void() + 
  labs(title = "Urban DCFC Chargers by Municipality")
ggsave("Urban DCFC Chargers by Municipality.png")

#Create neighbor list and weights 
coords <- st_coordinates(st_point_on_surface(urbanmunis))
#knn <- knearneigh(coords2, k = 6) #use k nearest neighbor to fix empty neighbor issue (for islands)
#nb <- knn2nb(knn)
#distance-band neighbors method
nb <- dnearneigh(coords, d1 = 0, d2 = 16093.4) #10 miles in meters
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

#Local Moran's I Method to compute hot spots statistics (returns I statistic, p-value, z-score, cluster type indicator)
#multiplies focal deviation by neighboring deviations, includes spatial outliers like high-low and low-high
local_moran <- localmoran(urbanmunis$n_dcfc, lw, zero.policy = TRUE) 
urbanmunis$I <- local_moran[,1] #adding I statistic to df
urbanmunis$P <- local_moran[,5] #adding p-value

x <- urbanmunis$n_dcfc
x_mean <- mean(x)
z <- scale(x)[,1] #mean standardized charger count of neighbors by I
lag_z <- lag.listw(lw, z)

urbanmunis$cluster <- "Not significant"
urbanmunis$cluster[urbanmunis$P < 0.10 & z > 0 & lag_z > 0] <- "High-High"
urbanmunis$cluster[urbanmunis$P < 0.10 & z < 0 & lag_z < 0] <- "Low-Low"
urbanmunis$cluster[urbanmunis$P < 0.10 & z > 0 & lag_z > 0] <- "High-Low"
urbanmunis$cluster[urbanmunis$P < 0.10 & z < 0 & lag_z < 0] <- "Low-High"
#classifies whether there is a high dcfc by high dcfc, low dcfc by low dcfc, or an insignificant result

#Creating map to visualize hotspots
nj <- states(cb = TRUE) |>
  filter(STUSPS == "NJ")
  st_transform(nj, st_crs(urbanmunis))
ggplot()+
  geom_sf(data = nj, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(data = urbanmunis, aes(fill= cluster), color = "grey30", size = 0.15) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "lightblue", 
      "Not significant" = "grey80"
      ),
    drop = FALSE
  ) +
  theme_minimal() + 
  labs(title = "Urban DCFC Hot Spot Analysis by Municipality (LISA)", fill = "Cluster Type")
ggsave("Urban DCFC Hot Spot Analysis by Municipality (LISA).png")


write_sf(urbanmunis, "Urban Municipality DCFC Hotspot Analysis.shp")
urbanmunis_df <-
  st_drop_geometry(urbanmunis)
write_csv(urbanmunis_df, "Urban Municipality DCFC Hotspot Analysis.csv")


##Count chargers by census blocks for L2
L2 <- chargers%>%
  filter(level == "L2")
#Pulling in census tracts
nj_blocks <- st_read("tl_2026_34_tabblock20.shp")
nj_blocks <- st_transform(nj_blocks, 3857)
census_urban_areas <- st_read("USA_Census_Urban_Areas.shp")
census_urban_areas <- st_transform(census_urban_areas, 3857)
#filtering to only urban blocks
urbanblocks <- st_intersection(nj_blocks, urbanmunis)|>
  st_make_valid() |>
  st_buffer(0)
urbanblocks <- urbanblocks %>% 
  mutate(n_L2 = 
           lengths(st_intersects(urbanblocks, L2))) 
urbanblocks$n_L2 <- as.numeric(urbanblocks$n_L2)

#preliminary visualization of charger counts
max <- max(urbanblocks$n_L2)
my_breaks <- c(0, 1, 2, 3, 4, 5, 25)
my_colors <- c("lightskyblue1", "skyblue", "deepskyblue1", "deepskyblue2", "deepskyblue3", "deepskyblue4", "darkblue", "black")
ggplot()+
  geom_sf(data = nj, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(data = urbanblocks, aes(fill= n_L2), color = "NA") +
  scale_fill_gradientn(
    name = "L2 Charger Count", 
      colors = my_colors,
      values = rescale(my_breaks, to = c(0, 1), from = c(0, 25)),
      breaks = c(0, 1, 2, 3, 4, 5, 25),
    #low = "white", 
    #mid = "lavender", 
    #high = "purple", 
    #midpoint = mean(urbanblocks$n_L2, na.rm = TRUE)) +
  )
  theme_void() + 
  labs(title = "Urban L2 Chargers by Census Block")
#ggsave("Urban L2 Chargers by Census Block.png", width = 8, height = 10, dpi = 300)

#Create neighbor list and weights 
urbanblocks_clean <- urbanblocks[!st_is_empty(urbanblocks), ]
coords2 <- st_coordinates(st_point_on_surface(urbanblocks_clean))
block_centroids <- st_centroid(st_geometry(urbanblocks_clean))
block_neighbors <- knn2nb(knearneigh(block_centroids, k = 6)) #use k nearest neighbor which works better for census blocks where dnearneigh would be distorted by large rural blocks
block_listw <- nb2listw(block_neighbors, style = "W")

#Local Moran's I Method to compute hot spots statistics (returns I statistic, p-value, z-score, cluster type indicator)
#multiplies focal deviation by neighboring deviations, includes spatial outliers like high-low and low-high
local_moran2 <- localmoran(urbanblocks_clean$n_L2, block_listw) 
urbanblocks_clean$I <- local_moran2[,1] #adding I statistic to df
urbanblocks_clean$P <- local_moran2[,5] #adding p-value

x2 <- urbanblocks_clean$n_L2
x2_mean <- mean(x2)
z2 <- scale(x2)[,1] #mean standardized charger count of neighbors by I
lag_z2 <- lag.listw(block_listw, z2)

urbanblocks_clean$cluster <- "Not significant"
urbanblocks_clean$cluster[urbanblocks_clean$P < 0.10 & z2 > 0 & lag_z2 > 0] <- "High-High"
urbanblocks_clean$cluster[urbanblocks_clean$P < 0.10 & z2 < 0 & lag_z2 < 0] <- "Low-Low"
urbanblocks_clean$cluster[urbanblocks_clean$P < 0.10 & z2 > 0 & lag_z2 > 0] <- "High-Low"
urbanblocks_clean$cluster[urbanblocks_clean$P < 0.10 & z2 < 0 & lag_z2 < 0] <- "Low-High"
#classifies whether there is a high dcfc by high dcfc, low dcfc by low dcfc, or an insignificant result

#Creating map to visualize hotspots
nj <- states(cb = TRUE) |>
  filter(STUSPS == "NJ")
nj <- st_transform(nj, st_crs(urbanblocks_clean))
ggplot()+
  geom_sf(data = nj, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(data = urbanblocks_clean, aes(fill= cluster), color = "NA", size = 0.05) +
  scale_fill_manual(
    values = c(
      "High-High" = "red",
      "Low-Low" = "blue",
      "High-Low" = "orange",
      "Low-High" = "lightblue", 
      "Not significant" = "grey80"
    ),
    drop = FALSE
  ) +
  theme_minimal() + 
  labs(title = "Urban L2 Hot Spot Analysis by Census Block (LISA)", fill = "Cluster Type")
#ggsave("Urban L2 Hot Spot Analysis by Census Block (LISA).png",  width = 8, height = 10, dpi = 300)

#write_sf(urbanblocks_clean, "Urban Census Block L2 Charger Hotspot Analysis.shp")
#urbanblocks_df <-
  #st_drop_geometry(urbanblocks_clean)
#write_csv(urbanblocks_df, "Urban Census Block L2 Charger Hotspot Analysis.csv")

#MUD percentage for housing data, also compare gas stations
#-----------------------------------------#
#Accessibility Analysis
library(osrm) 
library(dodgr) 
library(stplanr) 
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