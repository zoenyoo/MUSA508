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


# ROC Curve is useful because it visualizes trade-offs for two important confusion metrics
# Also is a goodness-of-fit indicator
ggplot(testProbs, aes(d = as.numeric(testProbs$Outcome), m = Probs)) +
  geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
  style_roc(theme = theme_grey) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  labs(title = "ROC Curve - churnModel")

# Area under the curve (AUC) measure: reasonable AUC is between 0.5 and 1
pROC::auc(testProbs$Outcome, testProbs$Probs)

# trainControl parameter is set to run 100 k-folds and to output predicted probabilities, classProbs, for ‘two classes’, Churn and No_Churn
ctrl <- trainControl(method = "cv", number = 100, classProbs=TRUE, summaryFunction=twoClassSummary)

# metrics in the cvFit output are for mean AUC, Sensitivity, and Specificity across all 100 folds.
cvFit <- train(Churn ~ ., data = churn %>% 
                 dplyr::select(
                   -SeniorCitizen, 
                   -WeekendBouncer,
                   -churnNumeric), 
               method="glm", family="binomial",
               metric="ROC", trControl = ctrl)

cvFit

# plots the distribution of AUC, Sensitivity, and Specificity across the 100 folds
# The tighter each distribution is to its mean, the more generalizable the model
dplyr::select(cvFit$resample, -Resample) %>%
  gather(metric, value) %>%
  left_join(gather(cvFit$results[2:4], metric, mean)) %>%
  ggplot(aes(value)) + 
  geom_histogram(bins=35, fill = "#FF006A") +
  facet_wrap(~metric) +
  geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Goodness of Fit", y="Count", title="CV Goodness of Fit Metrics",
       subtitle = "Across-fold mean reprented as dotted lines") +
  plotTheme()

# To calculate the total cost/benefit, these confusion metrics are multiplied by their corresponding costs (calculated in book)
cost_benefit_table <-
  testProbs %>%
  count(predOutcome, Outcome) %>%
  summarize(True_Negative = sum(n[predOutcome==0 & Outcome==0]),
            True_Positive = sum(n[predOutcome==1 & Outcome==1]),
            False_Negative = sum(n[predOutcome==0 & Outcome==1]),
            False_Positive = sum(n[predOutcome==1 & Outcome==0])) %>%
  gather(Variable, Count) %>%
  mutate(Revenue =
           case_when(Variable == "True_Negative"  ~ Count * 30,
                     Variable == "True_Positive"  ~ ((30 - 8) * (Count * .50)) + 
                       (-32 * (Count * .50)),
                     Variable == "False_Negative" ~ (-30) * Count,
                     Variable == "False_Positive" ~ (30 - 8) * Count)) %>%
  bind_cols(data.frame(Description = c(
    "We predicted no churn and did not send a mailer",
    "We predicted churn and sent the mailer",
    "We predicted no churn and the customer churned",
    "We predicted churn and the customer did not churn")))

# iterateThresholds iteratively loops through each threshold & calculates confusion metrics
whichThreshold <- 
  iterateThresholds(
    data=testProbs, observedClass = Outcome, predictedProbs = Probs)

whichThreshold[1:5,]

# Next, the result is moved to long form and Revenue is calculated for each confusion metric at each threshold
whichThreshold <- 
  whichThreshold %>%
  dplyr::select(starts_with("Count"), Threshold) %>%
  gather(Variable, Count, -Threshold) %>%
  mutate(Revenue =
           case_when(Variable == "Count_TN"  ~ Count * 30,
                     Variable == "Count_TP"  ~ ((30 - 8) * (Count * .50)) +
                       (-32 * (Count * .50)),
                     Variable == "Count_FN"  ~ (-30) * Count,
                     Variable == "Count_FP"  ~ (30 - 8) * Count))

# plots the Revenue for each confusion metric by threshold
whichThreshold %>%
  ggplot(.,aes(Threshold, Revenue, colour = Variable)) +
  geom_point() +
  scale_colour_manual(values = palette5[c(5, 1:3)]) +    
  labs(title = "Revenue by confusion matrix type and threshold",
       y = "Revenue") +
  plotTheme() +
  guides(colour=guide_legend(title = "Confusion Matrix")) 

# calculate the total Revenue across confusion metrics for each threshold
whichThreshold_revenue <- 
  whichThreshold %>% 
  mutate(actualChurn = ifelse(Variable == "Count_TP", (Count * .5),
                              ifelse(Variable == "Count_FN", Count, 0))) %>% 
  group_by(Threshold) %>% 
  summarize(Revenue = sum(Revenue),
            Actual_Churn_Rate = sum(actualChurn) / sum(Count),
            Actual_Churn_Revenue_Loss =  sum(actualChurn * 30),
            Revenue_Next_Period = Revenue - Actual_Churn_Revenue_Loss) 

# Revenue (this period) and Revenue_Next_Period are plotted below for each Threshold
whichThreshold_revenue %>%
  dplyr::select(Threshold, Revenue, Revenue_Next_Period) %>%
  gather(Variable, Value, -Threshold) %>%
  ggplot(aes(Threshold, Value, colour = Variable)) +
  geom_point() +
  geom_vline(xintercept = pull(arrange(whichThreshold_revenue, -Revenue)[1,1])) +
  scale_colour_manual(values = palette2) +
  plotTheme() + ylim(0,70000) +
  labs(title = "Revenue this pay period and the next by threshold",
       subtitle = "Assuming no new customers added next period. Vertical line denotes optimal threshold")
