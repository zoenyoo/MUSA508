
library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

policeDistricts <- 
  st_read("https://data.cityofchicago.org/api/geospatial/fthy-xz3r?method=export&format=GeoJSON") %>%
  st_transform('ESRI:102271') %>%
  dplyr::select(District = dist_num)

policeBeats <- 
  st_read("https://data.cityofchicago.org/api/geospatial/aerh-rz74?method=export&format=GeoJSON") %>%
  st_transform('ESRI:102271') %>%
  dplyr::select(District = beat_num)

bothPoliceUnits <- rbind(mutate(policeDistricts, Legend = "Police Districts"), 
                         mutate(policeBeats, Legend = "Police Beats"))

chicagoBoundary <- 
  st_read(file.path(root.dir,"/Chapter5/chicagoBoundary.geojson")) %>%
  st_transform('ESRI:102271') 

fishnet <- 
  st_make_grid(chicagoBoundary, cellsize = 500) %>%
  st_sf() %>%
  mutate(uniqueID = rownames(.))

burglaries <- 
  read.socrata("https://data.cityofchicago.org/Public-Safety/Crimes-2017/d62x-nvdr") %>% 
  filter(Primary.Type == "BURGLARY" & Description == "FORCIBLE ENTRY") %>%
  mutate(x = gsub("[()]", "", Location)) %>%
  separate(x,into= c("Y","X"), sep=",") %>%
  mutate(X = as.numeric(X),Y = as.numeric(Y)) %>% 
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102271') %>% 
  distinct()

crime_net <- 
  dplyr::select(burglaries) %>% 
  mutate(countBurglaries = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countBurglaries = replace_na(countBurglaries, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = crime_net, aes(fill = countBurglaries)) +
  scale_fill_viridis() +
  labs(title = "Count of Burglaires for the fishnet") +
  mapTheme()

abandonCars <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Abandoned-Vehicles/3c9v-pnva") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Cars")

abandonBuildings <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Vacant-and-Abandoned-Building/7nii-7srd") %>%
  mutate(year = substr(date_service_request_was_received,1,4)) %>%  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Buildings")

graffiti <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Graffiti-Removal-Historical/hec5-y4x5") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  filter(where_is_the_graffiti_located_ %in% c("Front", "Rear", "Side")) %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Graffiti")

streetLightsOut <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Street-Lights-All-Out/zuxi-7xem") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Street_Lights_Out")

sanitation <-
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Sanitation-Code-Complaints-Hi/me59-5fac") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Sanitation")

liquorRetail <- 
  read.socrata("https://data.cityofchicago.org/resource/nrmj-3kcf.json") %>%  
  filter(business_activity == "Retail Sales of Packaged Liquor") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Liquor_Retail")

neighborhoods <- 
  st_read("https://raw.githubusercontent.com/blackmad/neighborhoods/master/chicago.geojson") %>%
  st_transform(st_crs(fishnet)) 

vars_net <- 
  rbind(abandonCars,streetLightsOut,abandonBuildings,
        liquorRetail, graffiti, sanitation) %>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet) %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol=3, top="Risk Factors by Fishnet"))