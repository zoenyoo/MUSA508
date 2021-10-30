# Setup
options(scipen=10000000)

library(tidyverse)
library(caret)
library(knitr) 
library(pscl)
library(plotROC)
library(pROC)
library(scales)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#981FAC","#CB0F8B","#FF006A","#FE4C35","#FE9900")
palette4 <- c("#981FAC","#FF006A","#FE4C35","#FE9900")
palette2 <- c("#981FAC","#FF006A")

churn <- read.csv(file.path(root.dir,"/Chapter6/churnBounce.csv")) %>%
  mutate(churnNumeric = as.factor(ifelse(Churn == "Churn", 1, 0))) %>%
  na.omit()

# Exploratory Analysis
# Plot the mean for 2 continuous features, avgBounceTime and totalDistanceBounced, grouped by Churn or No_Churn
churn %>%
  dplyr::select(Churn,avgBounceTime, totalDistanceBounced) %>%
  gather(Variable, value, -Churn) %>%
  ggplot(aes(Churn, value, fill=Churn)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
  facet_wrap(~Variable, scales = "free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Mean", 
       title = "Feature associations with the likelihood of churn",
       subtitle = "(Continous outcomes)") +
  plotTheme() + theme(legend.position = "none")

# Plots below illustrate whether differences in customer factors associate with the likelihood that they will churn
churn %>%
  dplyr::select(Churn,SeniorCitizen, WeekendBouncer, bounceInStreet, PaperlessBilling) %>%
  gather(Variable, value, -Churn) %>%
  count(Variable, value, Churn) %>%
  filter(value == "Yes") %>%
  ggplot(aes(Churn, n, fill = Churn)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales = "free", ncol=4) +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Count",
       title = "Feature associations with the likelihood of churn",
       subtitle = "Two category features (Yes and No)") +
  plotTheme() + theme(legend.position = "none")

# Plot three category associations --> more experienced bouncers less likely to churn
churn %>%
  dplyr::select(Churn, phoneType, avgBounceDistance, avgBounceHeight) %>%
  gather(Variable, value, -Churn) %>%
  count(Variable, value, Churn) %>%
  ggplot(aes(value, n, fill = Churn)) +   
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~Variable, scales="free") +
  scale_fill_manual(values = palette2) +
  labs(x="Churn", y="Count",
       title = "Feature associations with the likelihood of churn",
       subtitle = "Three category features") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Training and test sets are created. createDataPartition is used to split the data. 
# A 50% sample is used here to reduce processing time
set.seed(3456)
trainIndex <- createDataPartition(churn$Churn, p = .50,
                                  list = FALSE,
                                  times = 1)
churnTrain <- churn[ trainIndex,]
churnTest  <- churn[-trainIndex,]

# Logistic regression model is estimated as churnreg1 (with the glm function)
# select operation is piped directly into glm to remove two features that are marginally significant as well as the Churn feature encoded as a string
churnreg1 <- glm(churnNumeric ~ .,
                     data=churnTrain %>% dplyr::select(-SeniorCitizen, 
                                                       -WeekendBouncer,
                                                       -Churn),
                     family="binomial" (link="logit"))

summary(churnreg1)

# Logistic regression coefficients are on the scale of ‘log-odds’
churn <- 
  mutate(churn, avgBounceDistance = ifelse(avgBounceDistance == "1-4 ft.", "1-4 ft.", 
                                           "Other"))

set.seed(3456)
trainIndex <- createDataPartition(churn$Churn, p = .50,
                                  list = FALSE,
                                  times = 1)
churnTrain <- churn[ trainIndex,]
churnTest  <- churn[-trainIndex,]

# avgBounceDistance is recoded in churnreg2 such that any value that does not equal 1-4 ft. receives Other
churnreg2 <- glm(churnNumeric ~ .,
                 data=churnTrain %>% dplyr::select(-SeniorCitizen, 
                                                   -WeekendBouncer,
                                                   -Churn),
                 family="binomial" (link="logit"))

summary(churnreg2)

# 'Pseudo R Squared’ Goodness-of-fit (weakest, but good for quickly comparing different model specifications)
pR2(churnreg2)[4]

# Goodness-of-fit predict for churnTest then tally up the rate that Churn and No_Churn are predicted correctly
# first step: create a data frame of test set probabilities, testProbs, which includes both the observed churn Outcome and predicted probabilities
testProbs <- data.frame(Outcome = as.factor(churnTest$churnNumeric),
                            Probs = predict(churnreg2, churnTest, type= "response"))
head(testProbs)

# Plot the distribution of predicted probabilities (x-axis) for Churn and No_Churn.
# If churnreg2 was very predictive, the ‘hump’ of predicted probabilities for Churn 
# would cluster around 1 on the x-axis, while the predicted probabilities for No_Churn would cluster around 0.
ggplot(testProbs, aes(x = Probs, fill = as.factor(Outcome))) + 
  geom_density() +
  facet_grid(Outcome ~ .) +
  scale_fill_manual(values = palette2) + xlim(0, 1) +
  labs(x = "Churn", y = "Density of probabilities",
       title = "Distribution of predicted probabilities by observed outcome") +
  plotTheme() + theme(strip.text.x = element_text(size = 18),
                      legend.position = "none")

# Create predOutcome that classifies any predicted probability greater than 0.50 as a predicted Churn event.
testProbs <- 
  testProbs %>%
  mutate(predOutcome  = as.factor(ifelse(testProbs$Probs > 0.5 , 1, 0)))

head(testProbs)

# Confusion matrix (value of 1 designates churn)
# 506 true positives (Sensitivity), 2306 true negatives (Specificity)
caret::confusionMatrix(testProbs$predOutcome, testProbs$Outcome, 
                           positive = "1")


ggplot(testProbs, aes(d = as.numeric(testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - churnModel")

