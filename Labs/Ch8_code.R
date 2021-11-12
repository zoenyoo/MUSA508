library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(gganimate)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(gifski)

options(tigris_class = "sf")
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c")
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")
palette2 <- c("#6baed6","#08519c")

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
ride <- read.csv(file.path(root.dir,"Chapter8/chicago_rideshare_trips_nov_dec_18_clean_sample.csv"))

#using lubridate - week, wkday and hour functions convert the data/time stamp into week of the year, day of the week, and hour of the day
ride2 <-
  ride %>% 
  mutate(interval60 = floor_date(mdy_hms(Trip.Start.Timestamp), unit = "hour"),
         interval15 = floor_date(mdy_hms(Trip.Start.Timestamp), unit = "15 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE),
         Pickup.Census.Tract = as.character(Pickup.Census.Tract),
         Dropoff.Census.Tract = as.character(Dropoff.Census.Tract)) %>%
  filter(Pickup.Census.Tract != "17031980000" & Pickup.Census.Tract != "17031770602")

ride2[1:3, c(1,4:7)]

# riem_measures function downloads weather.Data for O’Hare Airport
weather.Data <- 
  riem_measures(station = "ORD", date_start = "2018-11-01", date_end = "2019-01-01")

# weather.Panel is generated to summarize temperature, precipitation, and wind speed for every hour between November and December
# mutate_if and replace_na converts any character or numeric field with NA to 0
weather.Panel <-  
  weather.Data %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Percipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

# weather data is plotted as a time series using grid.arrange
grid.arrange(top = "Weather Data - Chicago - November & December, 2018",
                 ggplot(weather.Panel, aes(interval60,Percipitation)) + geom_line() + 
                   labs(title="Percipitation", x="Hour", y="Percipitation") + plotTheme(),
                 ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
                   labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
                 ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
                   labs(title="Temperature", x="Hour", y="Temperature") + plotTheme())

chicagoTracts <- 
  tigris::tracts(state = "Illinois", county = "Cook") %>%
  dplyr::select(GEOID) %>% filter(GEOID != 17031990000)

neighborhoodList <- 
  c("Grant Park","Printers Row","Loop","Millenium Park","West Loop","United Center",
    "West Town","East Village","Ukranian Village","Wicker Park","River North",
    "Rush & Division","Streeterville","Gold Coast","Old Town","Bucktown","Lincoln Park",
    "Sheffield & DePaul","Lake View","Boystown","Wrigleyville","North Center","Uptown", 
    "Lincoln Square","Little Italy, UIC")

nhoods <- 
  st_read("https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON") %>%
  st_transform(st_crs(chicagoTracts)) %>%
  filter(pri_neigh %in% neighborhoodList)

studyArea.tracts <-
  st_intersection(chicagoTracts, st_union(nhoods))

# ride.template filters for the 8 weeks of interest and uses semi_join to return only those trips in the studyArea.tract
ride.template <- 
  filter(ride2, week %in% c(45:52)) %>%
  semi_join(st_drop_geometry(studyArea.tracts), 
            by = c("Pickup.Census.Tract" = "GEOID"))

length(unique(ride.template$interval60)) * length(unique(ride.template$Pickup.Census.Tract))

# empty data frame, study.panel, is then created with the complete space/time panel. 
# This is done using the expand.grid function and unique
study.panel <- 
  expand.grid(interval60 = unique(ride.template$interval60), 
              Pickup.Census.Tract = unique(ride.template$Pickup.Census.Tract)) 

nrow(study.panel)      

# weather.Panel and studyArea.tracts are joined to provide weather and geometry information
ride.panel <- 
  ride.template %>%
  mutate(Trip_Counter = 1) %>%
  right_join(study.panel) %>% 
  group_by(interval60, Pickup.Census.Tract) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>%
  left_join(weather.Panel, by = "interval60") %>%
  left_join(studyArea.tracts, by = c("Pickup.Census.Tract" = "GEOID")) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label = TRUE)) %>%
  st_sf()

# additional feature engineering creates time lags - lag returns the Trip-Count for the previous nth time period
ride.panel <- 
  ride.panel %>% 
  arrange(Pickup.Census.Tract, interval60) %>% 
  group_by(Pickup.Census.Tract) %>% 
  mutate(lagHour = dplyr::lag(Trip_Count,1),
         lag2Hours = dplyr::lag(Trip_Count,2),
         lag3Hours = dplyr::lag(Trip_Count,3),
         lag4Hours = dplyr::lag(Trip_Count,4),
         lag12Hours = dplyr::lag(Trip_Count,12),
         lag1day = dplyr::lag(Trip_Count,24)) %>% 
  ungroup()

as.data.frame(filter(
  ride.panel, Pickup.Census.Tract == "17031831900"))[1:6, c(1:3,10:11)]

# training on 5 weeks of data, weeks 45-49, and testing on the following 3 weeks, 50-52
ride.Train <- filter(ride.panel, week < 50)
ride.Test <- filter(ride.panel, week >= 50)

# geom_vline is used to visualize mondays as well as Thanksgiving and Christmas
mondays <- 
  mutate(ride.panel,
         monday = ifelse(dotw == "Mon" & hour(interval60) == 1,
                         interval60, 0)) %>%
  filter(monday != 0) 

tg   <- as.POSIXct("2018-11-22 01:00:00 UTC")
xmas <- as.POSIXct("2018-12-24 01:00:00 UTC")

st_drop_geometry(rbind(
  mutate(ride.Train, Legend = "Training"), 
  mutate(ride.Test, Legend = "Testing"))) %>%
  group_by(Legend, interval60) %>% 
  summarize(Trip_Count = sum(Trip_Count)) %>%
  ungroup() %>% 
  ggplot(aes(interval60, Trip_Count, colour = Legend)) + geom_line() +
  scale_colour_manual(values = palette2) +
  geom_vline(xintercept = tg, linetype = "dotted") +
  geom_vline(xintercept = xmas, linetype = "dotted") +
  geom_vline(data = mondays, aes(xintercept = monday)) +
  labs(title="Rideshare trips by week: November-December",
       subtitle="Dotted lines for Thanksgiving & Christmas", 
       x="Day", y="Trip Count") +
  plotTheme() + theme(panel.grid.major = element_blank())    

# lag features are tested for correlation with Trip_Count
# plotData.lag returns the Trip_Count and time lag features for week 45. fct_relevel reorders the lag levels
plotData.lag <-
  filter(as.data.frame(ride.panel), week == 45) %>%
  dplyr::select(starts_with("lag"), Trip_Count) %>%
  gather(Variable, Value, -Trip_Count) %>%
  mutate(Variable = fct_relevel(Variable, "lagHour","lag2Hours","lag3Hours",
                                "lag4Hours","lag12Hours","lag1day"))
correlation.lag <-
  group_by(plotData.lag, Variable) %>%
  summarize(correlation = round(cor(Value, Trip_Count, use = "complete.obs"), 2)) 

# map tract Trip_Count sums by week and by day of the week, respectively. 
# Sum is chosen over mean here to avoid tract/time pairs with 0 counts
group_by(ride.panel, week, Pickup.Census.Tract) %>%
  summarize(Sum_Trip_Count = sum(Trip_Count)) %>%
  ungroup() %>% 
  ggplot() + geom_sf(aes(fill = q5(Sum_Trip_Count))) +
  facet_wrap(~week, ncol = 8) +
  scale_fill_manual(values = palette5,
                    labels = c("16", "140", "304", "530", "958"),
                    name = "Trip_Count") +
  labs(title="Sum of rideshare trips by tract and week") +
  mapTheme() + theme(legend.position = "bottom") 

# 15 minute intervals, interval15, are used from a single Monday in week45.panel
week45 <-
  filter(ride2 , week == 45 & dotw == "Mon")

week45.panel <-
  expand.grid(
    interval15 = unique(week45$interval15),
    Pickup.Census.Tract = unique(ride2$Pickup.Census.Tract))

# ride.animation.data panel is created
ride.animation.data <-
  mutate(week45, Trip_Counter = 1) %>%
  right_join(week45.panel) %>% 
  group_by(interval15, Pickup.Census.Tract) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>% 
  ungroup() %>% 
  left_join(chicagoTracts, by=c("Pickup.Census.Tract" = "GEOID")) %>%
  st_sf() %>%
  mutate(Trips = case_when(Trip_Count == 0 ~ "0 trips",
                           Trip_Count > 0 & Trip_Count <= 3 ~ "1-3 trips",
                           Trip_Count > 3 & Trip_Count <= 6 ~ "4-6 trips",
                           Trip_Count > 6 & Trip_Count <= 10 ~ "7-10 trips",
                           Trip_Count > 10 ~ "11+ trips")) %>%
  mutate(Trips  = fct_relevel(Trips, "0 trips","1-3 trips","4-6 trips",
                              "7-10 trips","10+ trips"))

# animation created
# transition_manual is set to interval15 to suggest a new map be generated for each 15 minute interval
rideshare_animation <-
  ggplot() +
  geom_sf(data = ride.animation.data, aes(fill = Trips)) +
  scale_fill_manual(values = palette5) +
  labs(title = "Rideshare pickups for one day in November 2018",
       subtitle = "15 minute intervals: {current_frame}") +
  transition_manual(interval15) +
  mapTheme()

animate(rideshare_animation, duration=20, renderer = gifski_renderer())

# save the gif 
anim_save("rideshare_local", rideshare_animation, duration=20, renderer = gifski_renderer())

# ride.panel includes three weather-related variables
# create a dummy variable for precipitation to see if it varies w rain
st_drop_geometry(ride.panel) %>%
  group_by(interval60) %>% 
  summarize(Trip_Count = mean(Trip_Count),
            Percipitation = first(Percipitation)) %>%
  mutate(isPercip = ifelse(Percipitation > 0,"Rain/Snow", "None")) %>%
  group_by(isPercip) %>%
  summarize(Mean_Trip_Count = mean(Trip_Count)) %>%
  ggplot(aes(isPercip, Mean_Trip_Count)) + geom_bar(stat = "identity") +
  labs(title="Does ridership vary with percipitation?",
       x="Percipitation", y="Mean Trip Count") +
  plotTheme()

# Plot temperature by ride count
st_drop_geometry(ride.panel) %>%
  group_by(interval60) %>% 
  summarize(Trip_Count = mean(Trip_Count),
            Temperature = first(Temperature)) %>%
  mutate(week = week(interval60)) %>%
  ggplot(aes(Temperature, Trip_Count)) + 
  geom_point() + geom_smooth(method = "lm", se= FALSE) +
  facet_wrap(~week, ncol=8) + 
  labs(title="Trip Count as a fuction of Temperature by week",
       x="Temperature", y="Mean Trip Count") +
  plotTheme() 

# re-engineering colinear_df as a nested tibble
colinear_nested <- nest(as_tibble(colinear_df), -time)
colinear_nested 

# can un-nest with:
unnest(colinear_nested[1,2])

# four different linear regressions are estimated on ride.Train
# reg1 focuses on just time, including hour fixed effects, day of the week, and Temperature
reg1 <- lm(Trip_Count ~  hour(interval60) + dotw + Temperature, data=ride.Train)

# reg2 focuses on just space effects with the Pickup.Census.Tract fixed effects
reg2 <- lm(Trip_Count ~  Pickup.Census.Tract + dotw + Temperature, data=ride.Train)

# reg3 includes both time and space fixed effects
reg3 <- lm(Trip_Count ~  Pickup.Census.Tract + hour(interval60) + dotw + Temperature, 
           data=ride.Train)

# reg4 adds the time lag features
reg4 <- lm(Trip_Count ~  Pickup.Census.Tract +  hour(interval60) + dotw + Temperature +
             lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day, 
           data=ride.Train)

# Mean Absolute Error (MAE) is calculated on ride.Test for each model. 
# ride.Test includes 3 weeks and is highly influenced by the Christmas holiday.
ride.Test.weekNest <- 
  as.data.frame(ride.Test) %>%
  nest(-week) 

ride.Test.weekNest

# small function is created that takes a tibble, dat and a regression model, fit as its inputs, and outputs predictions as pred
model_pred <- function(dat, fit){
  pred <- predict(fit, newdata = dat)}

# predictions are created by mapping the function, model_pred, to each row of data, parameterizing fit as the model.
week_predictions <- 
  ride.Test.weekNest %>% 
  mutate(A_Time_FE = map(.x = data, fit = reg1, .f = model_pred),
         B_Space_FE = map(.x = data, fit = reg2, .f = model_pred),
         C_Space_Time_FE = map(.x = data, fit = reg3, .f = model_pred),
         D_Space_Time_Lags = map(.x = data, fit = reg4, .f = model_pred))

# columns are moved to long form with gather & four new columns are generated in the code block below
week_predictions <- week_predictions %>%  
  gather(Regression, Prediction, -data, -week) %>% 
  mutate(Observed = map(data, pull, Trip_Count), # actual space/time Trip_Count for that week
         Absolute_Error = map2(Observed, Prediction,  ~ abs(.x - .y)),
         MAE = map_dbl(Absolute_Error, mean),
         sd_AE = map_dbl(Absolute_Error, sd))

# plot MAE by model by week (made easier by nesting)
week_predictions %>%
  dplyr::select(week, Regression, MAE) %>%
  gather(Variable, MAE, -Regression, -week) %>%
  ggplot(aes(week, MAE)) + 
  geom_bar(aes(fill = Regression), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette5) +
  labs(title = "Mean Absolute Errors by model specification and week") +
  plotTheme()
week_predictions 

# For each model, predicted and observed Trip_Count is taken out of the spatial context and their means plotted in time series form below
week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         Pickup.Census.Tract = map(data, pull, Pickup.Census.Tract)) %>%
  dplyr::select(interval60, Pickup.Census.Tract, Observed, Prediction, Regression) %>%
  unnest() %>%
  gather(Variable, Value, -Regression, -interval60, -Pickup.Census.Tract) %>%
  group_by(Regression, Variable, interval60) %>%
  summarize(Value = mean(Value)) %>%
  ggplot(aes(interval60, Value, colour=Variable)) + geom_line(size = 1.1) + 
  facet_wrap(~Regression, ncol=1) +
  scale_colour_manual(values = palette2) +
  labs(title = "Mean Predicted/Observed ride share by hourly interval", 
       x = "Hour", y= "Rideshare Trips") +
  plotTheme()

# MAE for reg4 is mapped, by un-nesting it from week_predictions
error.byWeek <-
  filter(week_predictions, Regression == "D_Space_Time_Lags") %>% 
  unnest() %>% st_sf() %>%
  dplyr::select(Pickup.Census.Tract, Absolute_Error, week, geometry) %>%
  gather(Variable, Value, -Pickup.Census.Tract, -week, -geometry) %>%
  group_by(Variable, Pickup.Census.Tract, week) %>%
  summarize(MAE = mean(Value))

# mean absolute trip count error by tract and hour 
error.byHour <-
  filter(week_predictions, Regression == "D_Space_Time_Lags") %>% 
  unnest() %>% 
  st_sf() %>%
  dplyr::select(Pickup.Census.Tract, Absolute_Error, geometry, interval60) %>%
  gather(Variable, Value, -interval60, -Pickup.Census.Tract, -geometry) %>%
  filter(wday(interval60, label = TRUE) == "Mon" & week(interval60) == 50) %>%
  group_by(hour = hour(interval60), Pickup.Census.Tract) %>%
  summarize(MAE = mean(Value)) 