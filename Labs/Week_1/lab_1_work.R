library(tidyverse)
library(tidycensus)
library(sf)

# Reference intro_to_tidycensus.html for more info on this tutorial

census_api_key("0e2fd0485a8e62ac3a5ebf1ad8bebb2cb181de6b", overwrite = TRUE, install = TRUE)

acs_variable_list.2016 <- load_variables(2016, #year
                                         "acs5", #five year ACS estimates
                                         cache = TRUE)

acs_variable_list.2010 <- load_variables(2010, #year
                                         "acs5", #five year ACS estimates
                                         cache = TRUE)

acs_vars <- c("B01001_001E", # ACS total Pop estimate
              "B25002_001E", # Estimate of total housing units
              "B25002_003E", # Number of vacant housing units
              "B19013_001E", # Median HH Income ($)
              "B02001_002E", # People describing themselves as "white alone"
              "B06009_006E") # Total graduate or professional degree

acsTractsPHL.2016 <- get_acs(geography = "tract",
                             year = 2016, 
                             variables = acs_vars, 
                             geometry = FALSE, 
                             state = "PA", 
                             county = "Philadelphia", 
                             output = "wide") 

acsTractsPHL.2016 <- acsTractsPHL.2016 %>%
  dplyr::select (GEOID, NAME, all_of(acs_vars))

acsTractsPHL.2016 <- acsTractsPHL.2016 %>%
  rename (total_pop.2016 = B01001_001E,
          total_HU.2016 = B25002_001E,
          total_vacant.2016 = B25002_003E,
          med_HH_Income.2016 = B19013_001E,
          total_White.2016 = B02001_002E,
          total_GradDeg.2016 = B06009_006E)
acsTractsPHL.2016 <- acsTractsPHL.2016 %>%
  mutate(vacancyPct.2016 = total_vacant.2016/total_HU.2016,
         pctWhite.2016 = total_White.2016/total_pop.2016)

#glimpse(acsTractsPHL.2016) #to see a basic summary

# use ?___ to get a basic summary e.g. ?get_acs

# You can also chain all these commands together, like here:
# You have to use the "Pipe": The operator %>% is known as the "pipe" and lets 
# you chain operations together - passing a dataframe along through different 
# operations.

acsTractsPHL.2010 <- get_acs(geography = "tract",
                             year = 2010, 
                             variables = acs_vars,
                             geometry = FALSE,
                             state = "PA", 
                             county = "Philadelphia",
                             output = "wide") %>%
  dplyr::select (GEOID, NAME, all_of(acs_vars)) %>% 
  rename (total_pop.2010 = B01001_001E,
          total_HU.2010 = B25002_001E,
          total_vacant.2010 = B25002_003E,
          med_HH_Income.2010 = B19013_001E,
          total_White.2010 = B02001_002E,
          total_GradDeg.2010 = B06009_006E) %>%
  mutate(vacancyPct.2010 = total_vacant.2010/total_HU.2010,
         pctWhite.2010 = total_White.2010/total_pop.2010)

allACS <- left_join(acsTractsPHL.2010, acsTractsPHL.2016, by= c("GEOID"))

allACS <- allACS %>%
  mutate(change_med_HH_Income = med_HH_Income.2016 - (med_HH_Income.2010 * 1.1007), 
         change_Grad_Degree_Pct = (total_GradDeg.2016/total_pop.2016)-(total_GradDeg.2010/total_pop.2010))

# This one is my new one:
allACS <- allACS %>%
  mutate(change_White_Pct = (total_White.2016/total_pop.2016)-(total_White.2010/total_pop.2010))

hist(allACS$change_med_HH_Income)
ggplot(allACS)+
  geom_histogram(aes(change_med_HH_Income))
ggplot(allACS)+
  geom_histogram(aes(change_med_HH_Income), binwidth = 5000)+
  labs(
    title = "Change in Philadelphia HH median income by tract, 2010-2016",
    caption = "Data: US Census Bureau, ACS 5-year estimates",
    x="Change in Med HH Income (2016 dollars)", 
    y="Number of tracts")
summaryTable <- allACS %>%
  summarize(mean_change_HH_Income = mean(change_med_HH_Income, na.rm = TRUE),
            med_change_HH_Income = median(change_med_HH_Income, na.rm = TRUE))
myTracts <- c("42101023500", 
              "42101023600", 
              "42101023700", 
              "42101025300", 
              "42101025400",
              "42101025500", 
              "42101025600", 
              "42101038800")

allACS <- allACS %>%
  mutate(mtAiry = ifelse(GEOID %in% myTracts, "MT AIRY", "REST OF PHILADELPHIA"))

summaryTable2 <- allACS %>%
  group_by(mtAiry) %>%
  summarize(mean_change_HH_Income = mean(change_med_HH_Income, na.rm = TRUE),
            med_change_HH_Income = median(change_med_HH_Income, na.rm = TRUE))
acsTractsPHL.2016.sf <- get_acs(geography = "tract",
                                year = 2016, 
                                variables = acs_vars, 
                                geometry = TRUE, 
                                state = "PA", 
                                county = "Philadelphia", 
                                output = "wide") %>% 
  dplyr::select (GEOID, NAME, all_of(acs_vars)) %>%
  rename (total_pop.2016 = B01001_001E,
          total_HU.2016 = B25002_001E,
          total_vacant.2016 = B25002_003E,
          med_HH_Income.2016 = B19013_001E,
          total_White.2016 = B02001_002E,
          total_GradDeg.2016 = B06009_006E) %>%
  mutate(vacancyPct.2016 = total_vacant.2016/total_HU.2016,
         pctWhite.2016 = total_White.2016/total_pop.2016) %>%
  mutate(mtAiry = ifelse(GEOID %in% myTracts, "MT AIRY", "REST OF PHILADELPHIA")) %>%
  st_as_sf(crs = 4326) # Turn shp into sf object and project as WGS84
ggplot()+
  geom_sf(data = acsTractsPHL.2016.sf, aes(fill = pctWhite.2016),
          color = "transparent")+
  geom_sf(data = acsTractsPHL.2016.sf %>%
            filter(mtAiry == "MT AIRY") %>%
            st_union(),
          color = "white",
          fill = "transparent")+
  labs(
    title = "Percentage of those identifying as 'white only' by tract",
    subtitle = "",
    caption = "Data: US Census Bureau, ACS 5-year estimates")
