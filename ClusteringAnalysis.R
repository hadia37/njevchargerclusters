setwd("~\njevchargerclusters")
library(tidyverse) #includes ggplot2
library(sf)
library(tigris)
library(dbscan)
library(pals)

library(dplyr)
library(stringr)
library(readxl)

#Loading Data
ev_stations <- read.csv("alt_fuel_stations (Aug 26 2026).csv")
municipality_pop <- read.csv("NJMunicipalityTotalPopB01003.csv")

#Cleaning datasets (lower case, removing suffixes); fixing for unincorporated communities 
ev_stations_clean <- ev_stations %>%
  mutate(
    zip_code_clean = str_pad(as.character(ZIP), width = 5, side = "left", pad = "0"),
    municipality_match = trimws(tolower(Municipality)),
    municipality_match = str_remove_all(municipality_match, "\\b(city|township|twp|borough|town|village)\\b"),
    municipality_match = str_squish(municipality_match)
  )

pop_clean <- municipality_pop %>%
  mutate(
    # Extract just the name before the first comma
    municipality_clean = str_split_i(County_Subdivision, ",", 1),
    # Standardize casing and strip out legal suffixes
    municipality_match = trimws(tolower(municipality_clean)),
    municipality_match = str_remove_all(municipality_match, "\\b(city|township|twp|borough|town|village)\\b"),
    municipality_match = str_squish(municipality_match),
    # Strip out commas from the population count and convert to numbers
    population_numeric = as.numeric(str_replace_all(`Total`, ",", ""))
  )

#Merge the datasets
ev_merged <- ev_stations_clean %>%
  left_join(pop_clean, by = "municipality_match")
write.csv(ev_merged, "EV Stations Merged with Municipality Population.csv")
#MANUALLY FIX UNINCORPORATED COMMUNITY NAs

ev_merged_clean <- read.csv("EV Stations Merged with Municipality Population.csv")
  
#Filter L1, create urban binary variable, and split
ev_classified <- ev_merged_clean %>%
  filter(EV_Level2_EVSE_Num > 0 | EV_DC_Fast_Count > 0) %>%
  mutate(
    is_urban = if_else(population_numeric >= 50000, 1, 0),
    charger_type = case_when(
      EV_DC_Fast_Count > 0   ~ "DCFC",
      EV_Level2_EVSE_Num > 0 ~ "L2"
    )
  )
# Split for separate cluster analyses
urban_stations <- ev_classified %>% filter(is_urban == 1)
rural_stations <- ev_classified %>% filter(is_urban == 0)
#Switch name for urban vs rural

#Convert to sf and reproject
chargers_sf <- st_as_sf(rural_stations, coords = c("Longitude", "Latitude"), crs=4326)
charger_reproj <- st_transform(chargers_sf, 3857)
coords <- st_coordinates(charger_reproj)

#Conduct DBSCAN, adds cluster ID to main df, eps=meter radius
db <- dbscan(coords, eps=10000, minPts = 3)
rural_stations$cluster <- db$cluster

#Create map and color classification by cluster ID

##Get the official New Jersey county boundary outlines 
nj_counties <- counties(state = "NJ", cb = TRUE, class = "sf")

##Convert cluster column to a factor so R treats it as distinct colors
rural_stations <- rural_stations %>%
  mutate(cluster = as.factor(cluster))

##Separate out the DBSCAN noise/outliers (typically classifies noise as cluster -1 or 0)
ev_stations_groups <- rural_stations %>% filter(cluster != "0")
ev_stations_noise <- rural_stations %>% filter(cluster == "0")

##Build the Geographic Map
num_clusters <- length(unique(ev_stations_groups$cluster))
dynamic_palette <- colorRampPalette(viridisLite::turbo(20))(num_clusters)

ggplot() +
  # Layer 1: Draw the New Jersey county boundaries
  geom_sf(data = nj_counties, fill="grey95", color = "grey60", size = 0.5) +
  
  # Layer 2: Draw the DBSCAN noise points (small, light grey, so they don't distract)
  geom_point(data = ev_stations_noise, aes(x = Longitude, y = Latitude), 
             color = "grey75", size = 1, alpha = 0.4) +
  
  # Layer 3: Draw the actual EV Charger Clusters
  geom_point(data = ev_stations_groups, aes(x = Longitude, y = Latitude, color = cluster), 
             size = 2.5, alpha = 0.8) + 

  # Styling and theme adjustments
  theme_void() + # Strips away background grid lines for a clean map look
  scale_color_manual(values = dynamic_palette) + # Safely handles any number of clusters up to 100+
  labs(
    title = "DBSCAN Cluster Analysis: NJ EV Charging Stations (Rural)",
    subtitle = "Colored points show high-density station clusters; light grey points indicate noise.",
    color = "Cluster ID",
    caption = "Source: US Census Bureau (Tigris) & DOE Alternative Fuels Data Center"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    legend.position = "right"
  )

#Count  stations in each cluster
cluster_summary <- rural_stations %>%
  group_by(cluster) %>%
  tally(name = "station_count") %>%
  arrange(desc(station_count))
  View(cluster_summary)