
#True Positive ("Sensitivity") - "The person was predicted to recidivate and actually recidivated."
#True Negative ("Specificity") - "The person was predicted not to recidivate and actually did not recidivate."
#False Positive - "The person was predicted to recidivate and actually did not recidivate."
#False Negative - "The person was predicted not to recidivate and actually did recidivate."

library(lubridate)
library(tidyverse)
library(caret)
library(kableExtra)
library(ModelMetrics)
library(plotROC)
library(knitr)
library(grid)
library(gridExtra)
library(QuantPsyc)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette_9_colors <- c("#FF2AD4","#E53AD8","#CC4ADC","#996AE5","#7F7BE9",
                      "#668BED","#33ABF6","#19BBFA","#00CCFF")
palette_3_colors <- c("#FF2AD4","#7F7BE9","#00CCFF")
palette_2_colors <- c("#FF2AD4", "#00CCFF")
palette_1_colors <- c("#00CCFF")

# Building the initial model
raw_data <- read.csv(file.path(root.dir,"Chapter7/compas-scores-two-years.csv"))

df <- 
  raw_data %>%
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(priors_count != "36") %>%
  filter(priors_count != "25") %>%
  mutate(length_of_stay = as.numeric(as.Date(c_jail_out) - as.Date(c_jail_in)),
         priors_count = as.factor(priors_count),
         Recidivated = as.factor(ifelse(two_year_recid == 1,"Recidivate","notRecidivate")),
         recidivatedNumeric = ifelse(Recidivated == "Recidivate", 1, 0),
         race2 = case_when(race == "Caucasian"        ~ "Caucasian",
                           race == "African-American" ~ "African-American", 
                           TRUE                       ~ "Other")) %>%
  dplyr::select(sex,age,age_cat,race,race2,priors_count,two_year_recid,r_charge_desc,
                c_charge_desc,c_charge_degree,r_charge_degree,juv_other_count,
                length_of_stay,priors_count,Recidivated,recidivatedNumeric) %>%
  filter(priors_count != 38)

# Most frequent initial charges
group_by(df, c_charge_desc) %>%
  summarize(count = n()) %>%
  mutate(rate = count / sum(count)) %>%
  arrange(-rate) %>% head(9) %>%
  ggplot(aes(reorder(c_charge_desc, rate, FUN = max), 
             rate, fill = c_charge_desc)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = palette_9_colors) +
  labs(x = "Charge", y = "Rate", title= "Most frequent initial charges") +
  plotTheme() + theme(legend.position = "none") 

# Visualize the rate of recidivism by race
df %>%
  group_by(Recidivated, race) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>% filter(Recidivated == "Recidivate") %>%
  ggplot(aes(reorder(race, -freq), freq)) +
  geom_bar(stat = "identity", position = "dodge", fill = palette_2_colors[2]) +
  labs(title = "Recidivism rate by race",
       y = "Rate", x = "Race") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Idea: if racial bias is baked into the training data, then controlling explicitly for race is not likely to remove it.
# data is split into a 75% training set and a 25% test set using dplyr 
train <- df %>% dplyr::sample_frac(.75)
train_index <- as.numeric(rownames(train))
test <- df[-train_index, ]

# Create regressions controlling and not controlling for race
reg.noRace <- glm(Recidivated ~ ., data = 
                    train %>% dplyr::select(sex, age, age_cat,
                                            juv_other_count, length_of_stay, 
                                            priors_count, Recidivated),
                  family = "binomial"(link = "logit"))

reg.withRace <- glm(Recidivated ~ ., data = 
                      train %>% dplyr::select(sex, age, age_cat, race,
                                              juv_other_count, length_of_stay, 
                                              priors_count, Recidivated),
                    family = "binomial"(link = "logit"))


# estimate another regression, the same as reg.withRace, but with race2 (only Af-Am/White/other) and without priors_count
reg.withRace2 <- glm(Recidivated ~ ., data = 
                       train %>% dplyr::select(sex, age, age_cat, race2,
                                               juv_other_count, length_of_stay, 
                                               Recidivated),
                     family = "binomial"(link = "logit"))
reg.withRace2

# African Americans are reported to have far higher prior rates than other races (shown below)
# So, race and priors_count tell the same story, and this colinearity renders race insignificant when both are included in the model.
# SO, as race plays no role in the usefulness of our model, reg.noRace is used for the remainder of the analysis.
group_by(df, race2) %>%
  summarize(averagePriors = mean(as.numeric(priors_count))) %>%
  ggplot(aes(race2, averagePriors, fill = race2)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Mean priors by race", y = "Mean Priors", x = "Race") +
  scale_fill_manual(values = palette_3_colors, name = "Recidivism") +
  plotTheme() + theme(legend.position = "none") 

# Calculate a predicted recidivism class, predClass, for any predicted probability over 0.50
testProbs <- 
  data.frame(class = test$recidivatedNumeric,
             probs = predict(reg.noRace, test, type = "response"),
             Race = test$race2)

# Contrast observed and predicted recidivism rates given the 50% threshold.
# underprediction is far more pronouced for Caucasians and other races, relative to African Americans.
mutate(testProbs, predClass = ifelse(probs >= .5, 1, 0)) %>%
  group_by(Race) %>%
  summarize(Observed.recidivism = sum(class) / n(),
            Predicted.recidivism = sum(predClass) / n()) %>%
  gather(Variable, Value, -Race) %>%
  ggplot(aes(Race, Value)) +
  geom_bar(aes(fill = Race), position="dodge", stat="identity") +
  scale_fill_manual(values = palette_3_colors) +
  facet_wrap(~Variable) +
  labs(title = "Observed and predicted recidivism", x = "Race", y = "Rate") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# iterateThresholds takes observedClass: data frame of predicted probabilities,
# predictedProbs: a column of predicted probabilities, and 
# group parameter: provides confusion metrics by race.
testProbs.thresholds <- 
  iterateThresholds(data=testProbs, observedClass = class, 
                    predictedProbs = probs, group = Race)

# Results are filtered for just the 50% threshold. 
# Accuracy and the confusion metrics as rates are selected out, converted to long form and then plotted as a grouped bar plot
filter(testProbs.thresholds, Threshold == .5)  %>%
  dplyr::select(Accuracy, Race, starts_with("Rate")) %>%
  gather(Variable, Value, -Race) %>%
  ggplot(aes(Variable, Value, fill = Race)) +
  geom_bar(aes(fill = Race), position = "dodge", stat = "identity") +
  scale_fill_manual(values = palette_3_colors) +
  labs(title="Confusion matrix rates by race",
       subtitle = "50% threshold", x = "Outcome",y = "Rate") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# ROC Curve across race
aucTable <- 
  testProbs %>% 
  group_by(Race) %>% 
  summarize(AUC = auc(class,probs)) %>% 
  mutate(AUC = as.character(round(AUC, 3))) 

mutate(testProbs.thresholds, pointSize = ifelse(Threshold == .48, 24, 16)) %>%
  ggplot(aes(Rate_FP, Rate_TP, colour=Race)) + 
  geom_point(aes(shape = pointSize)) + geom_line() + scale_shape_identity() +
  scale_color_manual(values = palette_3_colors) +
  geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
  annotation_custom(tableGrob(aucTable, rows = NULL), xmin = .65, xmax = 1, ymin = 0, ymax = .25) +
  labs(title="ROC Curves by race", x="False Positive Rate", y="True Positive Rate") +
  plotTheme()

