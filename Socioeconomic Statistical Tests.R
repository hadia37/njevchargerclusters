#Append classification to all socioeconomic data for  statistical tests (separate script)
setwd("~/BPU EV Charging Research Project")
library(dplyr)
library(readxl)
library(stringr)
library(tidyverse)

PublicTransportUse <- read_excel("MEANS OF TRANSPORT TO WORK (B08301).xlsx")
PublicTransportUse <- PublicTransportUse %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))
MHI <- read_excel("MEDIAN HOUSHOLD INCOME (B19013).xlsx")
MHI <- MHI %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))
PercentMinority <- read_excel("RACE (B02001).xlsx")
PercentMinority <- PercentMinority %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))
PopDensity <- read.csv("NJ_POP_DENSITY.csv")
PopDensity <- PopDensity %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))
EVadoption <- read.csv("Electric Vehicles on the Road.csv")
EVadoption <- EVadoption %>%
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))
GapClass <- read.csv("NJ_CHARGER_DENSITY.csv")
GapClass <- GapClass %>%t
  mutate(ZIP = as.character(ZIP)) %>%
  mutate(ZIP = str_pad(ZIP, width = 5, side = "left", pad = "0"))

#Combine MHI, PercentMinority, PopDensity, PublicTransportUse, EVadoption (Evs per 1k people)
#Append gap classification by zip code
SocioeconomicData_combined <- MHI %>%
  left_join(PercentMinority %>% select(ZIP, Minority), 
            by = c("ZIP" = "ZIP"))%>%
  left_join(PopDensity %>% select(ZIP, popdensity), 
            by = c("ZIP" = "ZIP")) %>%
  left_join(EVadoption %>% select(ZIP, EVs.per.1k.People), 
            by = c("ZIP" = "ZIP")) %>%
  left_join(PublicTransportUse %>% select(ZIP, "Public transportation"), 
            by = c("ZIP" = "ZIP")) %>%
  left_join(GapClass %>% select(ZIP, area_status), 
            by = c("ZIP" = "ZIP")) %>%
  mutate(`Public transportation` = str_remove(`Public transportation`, "%"), # Remove % if present
  `Public transportation` = as.numeric(`Public transportation`)       # Convert to numeric
)

View(SocioeconomicData_combined)
write_csv(SocioeconomicData_combined, "SocioeconomicData_combined.csv")

#Boxplot for MHI, PercentMinority, PopDensity, EVadoption
##Reshape continuous variables into long format
Socioeconomic_long <- SocioeconomicData_combined %>%
  select(MHI, Minority, popdensity, EVs.per.1k.People, "Public transportation", area_status) %>%
  pivot_longer(
    cols = -area_status, 
    names_to = "Factor", 
    values_to = "Value"
  )

##Plot boxplots with facets
ggplot(Socioeconomic_long, aes(x = area_status, y = Value, fill = area_status)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1) +
  facet_wrap(~Factor, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Socioeconomic Factors by Area Status",
    x = "Area Status",
    y = "Value",
    fill = "Status"
  )

##Significance testing (Wilcoxon)
wilcox.test(Minority ~ area_status, data = SocioeconomicData_combined)
wilcox.test(MHI ~ area_status, data = SocioeconomicData_combined)
wilcox.test(popdensity ~ area_status, data = SocioeconomicData_combined)
wilcox.test(EVs.per.1k.People ~ area_status, data = SocioeconomicData_combined)
wilcox.test(`Public transportation` ~ area_status, data = SocioeconomicData_combined)