if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
library(tidyverse)
library(tidymodels)
library(rsample)
if(!require('caret')) install.packages('caret')
library(caret)
if(!require('doParallel')) install.packages('doParallel')
library(doParallel)
library(pROC)

model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")

# trim some of the things out!
model_data_tbl <- model_data_tbl %>%
  mutate(home_winner_response = as.numeric(home_winner) %>% factor()
         , away_ap_rank_fct = factor(away_ap_rank_fct)
         , home_ap_rank_fct = factor(home_ap_rank_fct)
         , homeTeam_id = factor(homeTeam_id)
         , awayTeam_id = factor(awayTeam_id)
  )%>%
  select(
    contains("home_")
    , contains("away_")
    , awayTeam_id
    , homeTeam_id
    , -contains("city")
    , -contains("state")
    , -contains("arena")
    , -contains("_lat")
    , -contains("_lon")
    , -away_ap_rank
    , -home_ap_rank
    # , -ends_with("_")
    , -home_winner
    # , -away_ap_rank_fct
    # , -home_ap_rank_fct
  )

set.seed(15) # for reproducibility
initial_split_data <- initial_split(model_data_tbl, prop = .6)
train_tbl <- training(initial_split_data)
test_temp_tbl <- testing(initial_split_data)

set.seed(15)
validation_split <- initial_split(test_temp_tbl, prop = .5)
test_tbl <- training(validation_split)
validation_tbl <- testing(validation_split)

rm(initial_split_data, validation_split, test_temp_tbl) # run as needed

# Trying knocking out the rest of the factors that are mostly arbitrary
train_season_avg_wo_espn_tbl <- train_tbl %>%
  select(contains("home"), contains("away"), -contains("arena"), 
         -contains("lon"), -contains("lat"), -contains("roll5"),
         -home_espn_probability, -ends_with("_"))

train_roll5_wo_espn_tbl <- train_tbl %>%
  select(contains("home"), contains("away"), -contains("arena"), 
         -contains("lon"), -contains("lat"), -contains("season_avg"),
         -home_espn_probability, -ends_with("_"))


cl <- makePSOCKcluster(4)
doParallel::registerDoParallel(cl)

# Probit Model Season Average without ESPN predictions
probit_season_avg_model <- glm(home_winner_response ~ .,
                               family = binomial(link = "probit"),
                               data = train_season_avg_wo_espn_tbl)

summary(probit_season_avg_model)

# Create predictions
cutoff = 0.5
probit_season_avg_valid_predictions <- predict(probit_season_avg_model, newdata = validation_tbl, type = "response")
probit_season_avg_valid_predictions <- ifelse(probit_season_avg_valid_predictions >=0.5, 1, 0)

# Create confusion matrix
xtab <- table(probit_season_avg_valid_predictions, validation_tbl$home_winner_response)
result <- confusionMatrix(xtab)
result

# Accuracy, precision, specificity and AUC
probit_season_ave_model_accuracy <- result$overall['Accuracy']
probit_season_ave_model_precision <- result$byClass['Precision']
probit_season_ave_model_specificity <- result$byClass['Specificity']
probit_season_ave_model_auc <- auc(validation_tbl$home_winner_response, probit_season_avg_valid_predictions)
probit_season_ave_model_auc





# Probit Model Rolling Average of Last 5 Games without ESPN predictions
probit_roll5_model <- glm(home_winner_response ~ .,
                               family = binomial(link = "probit"),
                               data = train_roll5_wo_espn_tbl)

summary(probit_roll5_model)

# Create predictions
cutoff = 0.5
probit_roll5_valid_predictions <- predict(probit_roll5_model, newdata = validation_tbl, type = "response")
probit_roll5_valid_predictions <- ifelse(probit_roll5_valid_predictions >=0.5, 1, 0)

# Create confusion matrix
xtab <- table(probit_roll5_valid_predictions, validation_tbl$home_winner_response)
result <- confusionMatrix(xtab)
result

# Accuracy, precision, specificity and AUC
probit_roll5_model_accuracy <- result$overall['Accuracy']
probit_roll5_model_precision <- result$byClass['Precision']
probit_roll5_model_specificity <- result$byClass['Specificity']
probit_roll5_model_auc <- auc(validation_tbl$home_winner_response, probit_roll5_valid_predictions)
probit_roll5_model_auc









# Now performing stepwise for season average
library(MASS)
probit_season_ave_step_model <- probit_season_avg_model %>% stepAIC(trace=F)
coef(probit_season_ave_step_model)

# Predictions
probit_season_ave_step_model_pred <- predict(probit_season_ave_step_model, test_tbl, type = "response")
probit_season_ave_step_model_pred <- ifelse(probit_season_ave_step_model_pred >= 0.5, 1, 0)

# Create confusion matrix
xtab <- table(probit_season_avg_step_model_pred, test_tbl$home_winner_response)
result <- confusionMatrix(xtab)
result

# Accuracy, precision, specificity and AUC
probit_season_ave_step_model_accuracy <- result$overall['Accuracy']
probit_season_ave_step_model_precision <- result$byClass['Precision']
probit_season_ave_step_model_specificity <- result$byClass['Specificity']
probit_season_ave_step_model_auc <- auc(validation_tbl$home_winner_response, probit_season_avg_valid_predictions)
probit_season_ave_step_model_auc

# Perform stepwise for Roll5 data
probit_roll5_step_model <- probit_roll5_model %>% stepAIC(trace=F)
coef(probit_roll5_step_model)

# Predictions
probit_roll5_step_model_pred <- predict(probit_roll5_step_model, test_tbl, type = "response")
probit_roll5_step_model_pred <- ifelse(probit_roll5_step_model_pred >= 0.5, 1, 0)

# Create confusion matrix
xtab <- table(probit_roll5_step_model_pred, test_tbl$home_winner_response)
result <- confusionMatrix(xtab)
result

# Accuracy, precision, specificity and AUC
probit_roll5_step_model_accuracy <- result$overall['Accuracy']
probit_roll5_step_model_precision <- result$byClass['Precision']
probit_roll5_step_model_specificity <- result$byClass['Specificity']
probit_roll5_step_model_auc <- auc(validation_tbl$home_winner_response, probit_season_avg_valid_predictions)
probit_roll5_step_model_auc
