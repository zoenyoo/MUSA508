
library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)

palette5 <- c("#981FAC","#CB0F8B","#FF006A","#FE4C35","#FE9900")
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

census_api_key("0e2fd0485a8e62ac3a5ebf1ad8bebb2cb181de6b", overwrite = TRUE)

ride <- read.csv("https://raw.githubusercontent.com/zoenyoo/MUSA508/main/Ch8_HW/202103-04tripdata_sf.csv")

ride2 <-
  ride %>% 
  mutate(interval60 = floor_date(mdy_hm(started_at), unit = "hour"),
         interval15 = floor_date(mdy_hm(ended_at), unit = "15 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>% 

weather.Data <- 
  riem_measures(station = "SFO", date_start = "2021-03-01", date_end = "2021-04-30")

weather.Panel <-  
  weather.Data %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

sfCensus <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001",
                        "B08301_018"), 
          year = 2019, 
          state = "CA", 
          geometry = TRUE, 
          county=c("San Francisco"),
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E,
         Total_Bike = B08301_018E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans,
         Med_Age, Total_Bike,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport,
         Percent_Taking_bike = Total_Bike / Means_of_Transport)

sfTracts <- 
  sfCensus %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  select(GEOID, geometry) %>% 
  st_sf()

dat_census <- st_join(ride2 %>% 
                        filter(is.na(start_lng) == FALSE &
                                 is.na(start_lat) == FALSE &
                                 is.na(end_lng) == FALSE &
                                 is.na(end_lat) == FALSE) %>%
                        st_as_sf(., coords = c("start_lng", "start_lat"), crs = 4326),
                      sfTracts %>%
                        st_transform(crs=4326),
                      join=st_intersects,
                      left = TRUE) %>%
  rename(Origin.Tract = GEOID) %>%
  mutate(start_lng = unlist(map(geometry, 1)),
         start_lat = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)%>%
  st_as_sf(., coords = c("end_lng", "end_lat"), crs = 4326) %>%
  st_join(., sfTracts %>%
            st_transform(crs=4326),
          join=st_intersects,
          left = TRUE) %>%
  rename(Destination.Tract = GEOID)  %>%
  mutate(end_lng = unlist(map(geometry, 1)),
         end_lat = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)

grid.arrange(top = "Weather Data - San Francisco - March & April, 2021",
             ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
               labs(title="Precipitation", x="Hour", y="Precipitation") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
               labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
             ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
               labs(title="Temperature", x="Hour", y="Temperature") + plotTheme())

grid.arrange(ncol=2,
ggplot(dat_census %>%
         filter(interval60 >= as.Date("2021-03-01") & interval60 <= as.Date("2021-03-31")) %>% 
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="Bike Share Trips per Hour, San Francisco: \nMarch 2021",
       x="Date", 
       y="Number of trips")+
  plotTheme(),
ggplot(dat_census %>%
         filter(interval60 >= as.Date("2021-04-01") & interval60 <= as.Date("2021-04-30")) %>% 
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="\nApril 2021",
       x="Date", 
       y="Number of trips")+
  plotTheme())

dat_census %>%
  mutate(time_of_day = case_when(hour(interval60) < 5 | hour(interval60) > 20 ~ "Overnight (8p-4a)",
                                 hour(interval60) >= 5 & hour(interval60) < 10 ~ "Morning (5a-10a",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day (10a-3p)",
                                 hour(interval60) >= 15 & hour(interval60) <= 20 ~ "Evening (3p-8p)"))%>%
  group_by(interval60, start_station_name, time_of_day) %>%
  tally()%>%
  group_by(start_station_name, time_of_day)%>%
  summarize(mean_trips = mean(n))%>%
  ggplot()+
  geom_histogram(aes(mean_trips), binwidth = 1)+
  labs(title="Mean Number of Hourly Trips Per Station.",
       subtitle="San Francisco, March & April 2021",
       x="Number of trips", 
       y="Frequency")+
  facet_wrap(~time_of_day)+
  plotTheme()

ggplot(dat_census %>%
         group_by(hour(started_at), start_station_name) %>%
         tally())+
  geom_histogram(aes(n), binwidth = 1)+
  labs(title="Bike Share Trips per Hour by Station",
       subtitle="San Francisco, March & April 2021",
       x="Trip Counts", 
       y="Number of Stations")+
  plotTheme()

ggplot(dat_census %>% mutate(hour = hour(interval60)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Bike Share Trips by Day of the Week",
       subtitle="San Francisco, March & April 2021",
       x="Hour", 
       y="Trip Counts")+
  plotTheme()


ggplot(dat_census %>% 
         mutate(hour = hour(interval60),
                weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday")))+
  geom_freqpoly(aes(hour, color = weekend), binwidth = 1)+
  labs(title="Bike Share Trips in SF - Weekend vs. Weekday",
       subtitle="March & April 2021",
       x="Hour", 
       y="Trip Counts")+
  plotTheme()

ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs=4326))+
  geom_point(data = dat_census %>% 
               mutate(hour = hour(interval60),
                      weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
                      time_of_day = case_when(hour(interval60) < 5 | hour(interval60) > 20 ~ "Overnight (8p-4a)",
                                              hour(interval60) >= 5 & hour(interval60) < 10 ~ "Morning (5a-10a",
                                              hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day (10a-3p)",
                                              hour(interval60) >= 15 & hour(interval60) <= 20 ~ "Evening (3p-8p)"))%>%
               group_by(start_station_name, start_lat, start_lng, weekend, time_of_day) %>%
               tally(),
             aes(x=start_lng, y = start_lat, color = n), 
             fill = "transparent", alpha = 0.4, size = 0.3)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  ylim(min(dat_census$start_lat), max(dat_census$start_lat))+
  xlim(min(dat_census$start_lng), max(dat_census$start_lng))+
  facet_grid(weekend ~ time_of_day)+
  labs(title="Bike Share Trips per Hour by Station",
       subtitle="March & April 2021")+
  mapTheme()

