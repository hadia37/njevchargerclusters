setwd("~/BPU EV Charging Research Project")
library(dplyr)
library(readxl)  
library(readr)

#Loading data
NJ_totalpop <- read_excel("TOTAL POPULATION (B01003).xlsx", col_types = c("text", "numeric"))
national_ZCTA <- read_delim("2025_Gaz_zcta_national.txt",  delim = "|", col_types = cols(GEOID = "c", ALAND_SQMI = "d"))

#Ensure standard 5-digit strings
national_ZCTA <- national_ZCTA %>% 
  mutate(GEOID = sprintf("%05d", as.numeric(GEOID)))
NJ_totalpop <- NJ_totalpop %>% 
  mutate(ZIP = sprintf("%05d", as.numeric(ZIP)))

#Selecting Land Area and Joining by Matching Column
NJ_popdensity <- NJ_totalpop %>%
  left_join(
    national_ZCTA %>% select(GEOID, ALAND_SQMI), 
    by = c("ZIP" = "GEOID")
    
  )

#Calculating population density
NJ_popdensity <- NJ_popdensity %>%
  mutate(popdensity = `Total Population` / `ALAND_SQMI`)
View(NJ_popdensity)
write_csv(NJ_popdensity, "NJ_POP_DENSITY.csv")

#Calculating EV charger density and Classifying gap vs non-gap areas (<2 chargers/sq mi?)
EV_chargers <- read.csv("alt_fuel_stations (Aug 26 2026).csv", colClasses = c("ZIP" = "character"))
EV_chargers_clean <- EV_chargers %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))
NJ_popdensity_clean <-NJ_popdensity %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))

charger_density <- EV_chargers_clean %>%
  count(ZIP, name = "charger_count") %>%
  left_join(
    NJ_popdensity_clean %>%
    st_drop_geometry() %>%
    select(ZIP, ALAND_SQMI), 
             by = "ZIP") %>%
  mutate(
    charger_density = charger_count / as.numeric(ALAND_SQMI),
    area_status = case_when(
      charger_density < 1 | is.na(charger_density) ~ "Gap",
      TRUE                                           ~ "Non-Gap"
    )
    )
View(charger_density)
write_csv(charger_density, "NJ_CHARGER_DENSITY.csv")


#Make a separate map for gap vs non-gap areas by zip codes 
library(tidyverse)
library(sf)

#Load  and join ZCTA shapefile, filter with inner_join to only keep NJ zip codes
us_shapes <- st_read("tl_2020_us_zcta520.shp")
charger_gap_map_data <- us_shapes %>%
  inner_join(charger_density, by = c("GEOID20" = "ZIP")) %>% #Census shapefiles usually name the 5-digit ZIP code column "GEOID20" or "ZCTA5CE20"
  st_make_valid()%>%
  filter(!st_is_empty(.))%>%
  filter(st_is_valid(.))
charger_gap_map_data <-charger_gap_map_data %>%
  filter(st_coordinates(st_centroid(geometry))[,1] > -77,
  st_coordinates(st_centroid(geometry))[,1] < -73,
  st_coordinates(st_centroid(geometry))[,2] > 38,
  st_coordinates(st_centroid(geometry))[,2] < 42)
  
#Generate the Map
ggplot(data = charger_gap_map_data) +
  # Draw the zip code boundaries and color them by your classification
  geom_sf(aes(fill = area_status), color = "white", size = 0.1) +
  # Apply distinct colors for clear visualization
  scale_fill_manual(
    values = c("Gap" = "#e41a1c", "Non-Gap" = "#377eb8"),
    name = "EV Infrastructure Status",
    na.value = "#cccccc" # Light gray for zip codes with missing data
  ) +
  # Strip away standard chart grid lines for a clean map look
  theme_void() +
  labs(
    title = "EV Charger Gap Analysis by Zip Code",
    subtitle = "Gap defined as < 1 charger per square mile",
    caption = "Data Source: Alternative Fuel Stations & NJ Census"
  )