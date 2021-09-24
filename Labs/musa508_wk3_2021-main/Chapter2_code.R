
Hideoptions(scipen=999)

library(tidyverse)
library(sf)
library(gridExtra)
library(grid)
library(kableExtra)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

lancCounty <- st_read(file.path(root.dir,"/Chapter2/LancasterCountyBoundary.geojson")) %>%
  st_transform('ESRI:102728')

uga <- st_read(file.path(root.dir,"/Chapter2/Urban_Growth_Boundary.geojson")) %>% 
  st_transform('ESRI:102728')

studyAreaTowns <- st_read(file.path(root.dir,"/Chapter2/StudyAreaTowns.geojson")) %>% 
  st_transform('ESRI:102728')
buildings <- st_read(file.path(root.dir,"/Chapter2/LancasterCountyBuildings.geojson")) %>% 
  st_transform('ESRI:102728')
greenSpace <- st_read(file.path(root.dir,"/Chapter2/LancasterGreenSpace.geojson")) %>% 
  st_transform('ESRI:102728')

uga_union <- 
  st_union(uga) %>%
  st_buffer(1) %>%
  st_sf()

outsideBuffer <-
  st_buffer(uga_union, 660) %>%
  st_difference(uga_union) %>%
  mutate(Legend = "Outside")

insideBuffer <- 
  st_buffer(uga_union, dist = -660) %>%
  st_difference(uga_union, .) %>%
  mutate(Legend = "Inside")

bothBuffers <- rbind(insideBuffer, outsideBuffer)

ggplot() + 
  geom_sf(data = bothBuffers, aes(fill = Legend)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) + 
  labs(title = "1/8mi buffer inside & outside UGA") +
  mapTheme()

buffersAndTowns <- 
  st_intersection(st_make_valid(studyAreaTowns), bothBuffers) %>%
  filter(MUNI != "MOUNTVILLE BOROUGH" & MUNI != "MILLERSVILLE BOROUGH")

# Can you create this image? https://urbanspatial.github.io/PublicPolicyAnalytics/03_UGB_files/figure-html/30_plotTownsandBuffers-1.png

buildingCentroids <- 
  st_centroid(buildings) %>%
  mutate(counter = 1) %>% 
  dplyr::select(counter)

buffersAndTowns_Buildings <- 
  aggregate(buildingCentroids, buffersAndTowns, sum) %>%
  cbind(buffersAndTowns) %>%
  mutate(counter = replace_na(counter, 0),
         Area = as.numeric(st_area(.))) 

buffersAndTowns_Buildings_Summarize <-
  buffersAndTowns_Buildings %>%
  group_by(MUNI, Legend) %>%
  summarize(Building_Count = sum(counter),
            Area = sum(Area)) %>%
  mutate(Building_Density = Building_Count / Area)  

buildingDifferenceTable <-
  st_drop_geometry(buffersAndTowns_Buildings_Summarize) %>%
  dplyr::select(MUNI, Legend, Building_Density) %>%
  spread(Legend, Building_Density) %>%
  mutate(Building_Difference = Inside - Outside) %>%
  arrange(desc(Building_Difference)) 

