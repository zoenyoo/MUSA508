library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(scales)

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

boston.sf <- st_read(file.path(root.dir,"/Chapter3_4/boston_sf_Ch1_wrangled.geojson")) %>% 
  st_set_crs('ESRI:102286')

nhoods <- 
  st_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson") %>%
  st_transform('ESRI:102286')

inTrain <- createDataPartition(
  y = paste(boston.sf$Name, boston.sf$NUM_FLOORS.cat, 
            boston.sf$Style, boston.sf$R_AC), 
  p = .60, list = FALSE)
boston.training <- boston.sf[inTrain,] 
boston.test <- boston.sf[-inTrain,]  

reg.training <- 
  lm(SalePrice ~ ., data = as.data.frame(boston.training) %>% 
       dplyr::select(SalePrice, LivingArea, Style, 
                     GROSS_AREA, NUM_FLOORS.cat,
                     R_BDRMS, R_FULL_BTH, R_HALF_BTH, 
                     R_KITCH, R_AC, R_FPLACE, crimes.Buffer))

boston.test <-
  boston.test %>%
  mutate(Regression = "Baseline Regression",
         SalePrice.Predict = predict(reg.training, boston.test),
         SalePrice.Error = SalePrice.Predict - SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict - SalePrice),
         SalePrice.APE = (abs(SalePrice.Predict - SalePrice)) / SalePrice.Predict)%>%
  filter(SalePrice < 5000000) 

coords <- st_coordinates(boston.sf) 

neighborList <- knn2nb(knearneigh(coords, 5))

spatialWeights <- nb2listw(neighborList, style="W")

boston.sf$lagPrice <- lag.listw(spatialWeights, boston.sf$SalePrice)

coords.test <-  st_coordinates(boston.test) 

neighborList.test <- knn2nb(knearneigh(coords.test, 5))

spatialWeights.test <- nb2listw(neighborList.test, style="W")

boston.test %>% 
  mutate(lagPriceError = lag.listw(spatialWeights.test, SalePrice.Error)) %>%
  ggplot(aes(lagPriceError, SalePrice.Error))

moranTest <- moran.mc(boston.test$SalePrice.Error, 
                          spatialWeights.test, nsim = 999)

ggplot(as.data.frame(moranTest$res[c(1:999)]), aes(moranTest$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic), colour = "#FA7800",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in orange",
       x="Moran's I",
       y="Count") +
  plotTheme()

left_join(
  st_drop_geometry(boston.test) %>%
    group_by(Name) %>%
    summarize(meanPrice = mean(SalePrice, na.rm = T)),
  mutate(boston.test, predict.fe = 
           predict(lm(SalePrice ~ Name, data = boston.test), 
                   boston.test)) %>%
    st_drop_geometry %>%
    group_by(Name) %>%
    summarize(meanPrediction = mean(predict.fe))) %>%
  kable() %>% kable_styling()

reg.nhood <- lm(SalePrice ~ ., data = as.data.frame(boston.training) %>% 
                  dplyr::select(Name, SalePrice, LivingArea, 
                                Style, GROSS_AREA, NUM_FLOORS.cat,
                                R_BDRMS, R_FULL_BTH, R_HALF_BTH, 
                                R_KITCH, R_AC, R_FPLACE,crimes.Buffer))

boston.test.nhood <-
  boston.test %>%
  mutate(Regression = "Neighborhood Effects",
         SalePrice.Predict = predict(reg.nhood, boston.test),
         SalePrice.Error = SalePrice.Predict- SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict- SalePrice),
         SalePrice.APE = (abs(SalePrice.Predict- SalePrice)) / SalePrice)%>%
  filter(SalePrice < 5000000)

bothRegressions <- 
  rbind(
    dplyr::select(boston.test, starts_with("SalePrice"), Regression, Name) %>%
      mutate(lagPriceError = lag.listw(spatialWeights.test, SalePrice.Error)),
    dplyr::select(boston.test.nhood, starts_with("SalePrice"), Regression, Name) %>%
      mutate(lagPriceError = lag.listw(spatialWeights.test, SalePrice.Error)))   

st_drop_geometry(bothRegressions) %>%
  gather(Variable, Value, -Regression, -Name) %>%
  filter(Variable == "SalePrice.AbsError" | Variable == "SalePrice.APE") %>%
  group_by(Regression, Variable) %>%
  summarize(meanValue = mean(Value, na.rm = T)) %>%
  spread(Variable, meanValue) %>%
  kable()

bothRegressions %>%
  dplyr::select(SalePrice.Predict, SalePrice, Regression) %>%
  ggplot(aes(SalePrice, SalePrice.Predict)) +
  geom_point() +
  stat_smooth(aes(SalePrice, SalePrice), 
              method = "lm", se = FALSE, size = 1, colour="#FA7800") + 
  stat_smooth(aes(SalePrice.Predict, SalePrice), 
              method = "lm", se = FALSE, size = 1, colour="#25CB10") +
  facet_wrap(~Regression) +
  labs(title="Predicted sale price as a function of observed price",
       subtitle="Orange line represents a perfect prediction; Green line represents prediction") +
  plotTheme()

st_drop_geometry(bothRegressions) %>%
  group_by(Regression, Name) %>%
  summarize(mean.MAPE = mean(SalePrice.APE, na.rm = T)) %>%
  ungroup() %>% 
  left_join(nhoods) %>%
  st_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = mean.MAPE)) +
  geom_sf(data = bothRegressions, colour = "black", size = .5) +
  facet_wrap(~Regression) +
  scale_fill_gradient(low = palette5[1], high = palette5[5],
                      name = "MAPE") +
  labs(title = "Mean test set MAPE by neighborhood") +
  mapTheme()

tracts17 <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E","B06011_001"), 
          year = 2017, state=25, county=025, geometry=T, output = "wide") %>%
  st_transform('ESRI:102286')  %>%
  rename(TotalPop = B01001_001E,
         NumberWhites = B01001A_001E,
         Median_Income = B06011_001E) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority White", "Majority Non-White"),
         incomeContext = ifelse(Median_Income > 32322, "High Income", "Low Income"))

grid.arrange(ncol = 2,
             ggplot() + geom_sf(data = na.omit(tracts17), aes(fill = raceContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Race Context") +
               labs(title = "Race Context") +
               mapTheme() + theme(legend.position="bottom"), 
             ggplot() + geom_sf(data = na.omit(tracts17), aes(fill = incomeContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Income Context") +
               labs(title = "Income Context") +
               mapTheme() + theme(legend.position="bottom"))

st_join(bothRegressions, tracts17) %>% 
  group_by(Regression, raceContext) %>%
  summarize(mean.MAPE = scales::percent(mean(SalePrice.APE, na.rm = T))) %>%
  st_drop_geometry() %>%
  spread(raceContext, mean.MAPE) %>%
  kable(caption = "Test set MAPE by neighborhood racial context")

st_join(bothRegressions, tracts17) %>% 
  filter(!is.na(incomeContext)) %>%
  group_by(Regression, incomeContext) %>%
  summarize(mean.MAPE = scales::percent(mean(SalePrice.APE, na.rm = T))) %>%
  st_drop_geometry() %>%
  spread(incomeContext, mean.MAPE) %>%
  kable(caption = "Test set MAPE by neighborhood income context")
