library(spdep) 
library(tidyverse)
library(tigris)
library(sf)

#Read data for number of establishments by zip code
business <- read.csv("CBP2023.CB2300CBP-2026-10-22T140912.csv")
business <- business %>%
  mutate(
    num_est = as.numeric(str_remove_all(Number.of.establishments, ",")),
    zip_clean = str_extract(Geographic.Area.Name, "\\d{5}")
    ) %>%
  filter(!is.na(zip_clean), !is.na(num_est)) %>%
  group_by(zip_clean) %>%
  summarize(total_est = sum(num_est, na.rm = TRUE), .groups = "drop")

nj_zips <- zctas(state = "NJ", year = 2010) %>%
  mutate(ZCTA5CE10 = trimws(as.character(ZCTA5CE10)))
nj_zips <- nj_zips %>%
  left_join(business, by = c("ZCTA5CE10" = "zip_clean"))

nj_zips$total_est <- as.numeric(as.character(nj_zips$total_est))
nj_zips$total_est[is.na(nj_zips$total_est)] <- 0

nj_zips <- nj_zips %>%
  mutate(total_est = coalesce(total_est, 0)) #for any unmatched ZIPs
nj_zips <- nj_zips[!st_is_empty(nj_zips), ] #removing empty geometries
nj_zips <- nj_zips[!is.na(st_dimension(nj_zips)), ]


#Hot spot analysis 
#Create neighbor list and weights 

coords <- st_coordinates(st_centroid(nj_zips))
knn <- knearneigh(coords, k = 6)
nb <- knn2nb(knn)
#nb <- poly2nb(nj_zips)
lw <- nb2listw(nb, style = "W")

#Getis-Ord GI* Method to compute hot spots statistics
#quick fix making NAs 0s
gi_results <- localG(nj_zips$total_est, lw) 
nj_zips$GiZScore <- as.numeric(gi_results) #add Gi Z score to data

#Classification
nj_zips <- nj_zips %>%
  mutate(GiCategory = case_when(GiZScore >= 2.58 ~ "Hotspot (99% confidence)", GiZScore >= 1.96 ~ "Hotspot (95% confidence)",
                                GiZScore <= -2.58 ~ "Coldspot (99% confidence)", GiZScore <= -1.96 ~ "Coldspot (95% confidence)",
                                TRUE ~ "Not significant" ))
st_write(nj_zips, "NJ ZIP Workplace Hotspot Analysis.shp")
nj_zips_no_geom <- st_drop_geometry(nj_zips)
write.csv(nj_zips_no_geom, "NJ ZIP Workplace Hotspot Analysis.csv")

nj <- states(cb = TRUE) |>
  filter(STUSPS == "NJ")
st_transform(nj, st_crs(nj_zips))
ggplot()+
  #geom_sf(data = nj, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(data = nj_zips, aes(fill = GiCategory), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c(
      "Hotspot (99% confidence)" = "red4", 
      "Hotspot (95% confidence)" = "red", 
      "Coldspot (99% confidence" = "blue4", 
      "Coldspot (95% confidence" = "blue", 
      "Not significant" = "grey80"
    )
  ) +
  theme_minimal() + 
  labs(title = "NJ Workplace Hotspot Analysis", fill = "Gi Classification")
ggsave("NJ Workplace Hotspot Analysis.png")