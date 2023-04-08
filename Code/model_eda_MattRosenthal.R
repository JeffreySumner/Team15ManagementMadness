rm(list=ls()) 
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('caret')) install.packages('caret')
if (!require('ROCR')) install.packages('ROCR')

model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")

model_data_tbl <- model_data_tbl %>%
  mutate(home_winner_response = as.numeric(home_winner))

model_data_tbl <- model_data_tbl %>%
  mutate(home_winner_response = as.numeric(home_winner) %>% factor()
         , away_ap_rank_fct = factor(away_ap_rank_fct)
         , home_ap_rank_fct = factor(home_ap_rank_fct)
         , homeTeam_id = factor(homeTeam_id)
         , awayTeam_id = factor(awayTeam_id)
  ) %>%
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

#rm(initial_split_data, validation_split, test_temp_tbl) # run as needed

# All Rolling Avg Predictors
train_tbl_roll5 <- train_tbl %>% select(contains("roll5") | 
                                        "home_winner_response") %>%
  select(-contains("opp"))
model_all_roll5 <- glm(home_winner_response ~., 
                           data = train_tbl_roll5, family = "binomial")
val_all_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_all_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_all_roll5$home_winner_response), 
                      data = factor(val_all_roll5$pred_WL), positive = "1")
predictions <- prediction(val_all_roll5$pred_prob, val_all_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- data.frame("type" = "all_roll5",
                        "accuracy" = cm$overall['Accuracy'],
                        "precision" = precision(data = factor(val_all_roll5$pred_WL),
                                                reference = factor(val_all_roll5$home_winner_response),
                                                positive = "1"),
                        "specificity" = cm$byClass['Sensitivity'],
                        "AUC" = auc_ROCR)
modelList <- list(model_all_roll5)
cmList <- list(cm)

# Significant Rolling Avg Predictors from Overall Model
model_sig_roll5 <- glm(home_winner_response ~ home_assists_roll5 +
                         home_blocks_roll5 +
                         home_defensive_rating_roll5 +
                         home_defensive_rebounds_roll5 +
                         home_FGA3_roll5 + home_FGM3_roll5 +
                         home_flagrant_fouls_roll5 +
                         home_offensive_rating_roll5 +
                         home_team_turnovers_roll5 +
                         away_blocks_roll5 +
                         away_defensive_rating_roll5 +
                         away_defensive_rebounds_roll5 +
                         away_FGA3_roll5 + away_FGM3_roll5 +
                         away_flagrant_fouls_roll5 + away_fouls_roll5 +
                         away_offensive_rating_roll5, 
                       data = train_tbl_roll5, family = "binomial")
val_sig_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_sig_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_sig_roll5$home_winner_response), 
                      data = factor(val_sig_roll5$pred_WL), positive = "1")
predictions <- prediction(val_sig_roll5$pred_prob, val_sig_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "sig_roll5",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_sig_roll5$pred_WL),
                                                           reference = factor(val_sig_roll5$home_winner_response),
                                                           positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list(model_sig_roll5))
cmList <- c(cmList, list(cm))

# Rolling Avg Predictors - Stepwise
if (!require('MASS')) install.packages('MASS')
model_step_roll5 <- model_all_roll5 %>% stepAIC(trace = FALSE)
detach("package:MASS", unload=TRUE)
val_step_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_step_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_step_roll5$home_winner_response), 
                      data = factor(val_step_roll5$pred_WL), positive = "1")
predictions <- prediction(val_step_roll5$pred_prob, val_step_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "stepwise_roll5",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_step_roll5$pred_WL),
                                                         reference = factor(val_step_roll5$home_winner_response),
                                                         positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list(model_step_roll5))
cmList <- c(cmList, list(cm))

# if (!require('corrplot')) install.packages('corrplot')
# step.cor <- cor(train_tbl[, attr(model_step_roll5$terms , "term.labels")])
# corrplot(step.cor)

# Rolling Avg Offensive / Defensive Rating model
model_ratings_roll5 <- glm(home_winner_response ~ home_offensive_rating_roll5 + 
                              home_defensive_rating_roll5 + 
                              away_offensive_rating_roll5 + 
                              away_defensive_rating_roll5, 
                            data = train_tbl, family = "binomial")
val_ratings_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_ratings_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_ratings_roll5$home_winner_response), 
                data = factor(val_ratings_roll5$pred_WL), positive = "1")
predictions <- prediction(val_ratings_roll5$pred_prob, val_ratings_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "ratings_roll5",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_ratings_roll5$pred_WL),
                                                         reference = factor(val_ratings_roll5$home_winner_response),
                                                         positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list(model_ratings_roll5))
cmList <- c(cmList, list(cm))

# Rolling Avg Offensive Rating model
model_off_ratings_roll5 <- glm(home_winner_response ~ home_offensive_rating_roll5 + 
                              away_offensive_rating_roll5, 
                            data = train_tbl, family = "binomial")
val_off_ratings_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_off_ratings_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_off_ratings_roll5$home_winner_response), 
                data = factor(val_off_ratings_roll5$pred_WL), positive = "1")
predictions <- prediction(val_off_ratings_roll5$pred_prob, val_off_ratings_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "off_ratings_roll5",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_off_ratings_roll5$pred_WL),
                                                         reference = factor(val_off_ratings_roll5$home_winner_response),
                                                         positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list(model_off_ratings_roll5))
cmList <- c(cmList, list(cm))

# Rolling Avg Defensive Rating model
model_def_ratings_roll5 <- glm(home_winner_response ~ home_defensive_rating_roll5 + 
                                  away_defensive_rating_roll5, 
                                data = train_tbl, family = "binomial")
val_def_ratings_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_def_ratings_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_def_ratings_roll5$home_winner_response), 
                data = factor(val_def_ratings_roll5$pred_WL), positive = "1")
predictions <- prediction(val_def_ratings_roll5$pred_prob, val_def_ratings_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "def_ratings_roll5",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_def_ratings_roll5$pred_WL),
                                                         reference = factor(val_def_ratings_roll5$home_winner_response),
                                                         positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list(model_def_ratings_roll5))
cmList <- c(cmList, list(cm))

# Rolling Avg Offense Model
model_offense_roll5 <- glm(home_winner_response ~
                             home_assists_roll5 + away_assists_roll5 +
                             home_blocks_roll5 + away_blocks_roll5 +
                             home_total_rebounds_roll5 + away_total_rebounds_roll5 +
                             home_FGM_roll5 / home_FGA_roll5 +
                             away_FGM_roll5 / away_FGA_roll5,  
                           data = train_tbl, family = "binomial")
val_offense_roll5 <- validation_tbl %>%
  mutate(pred_prob = predict(model_offense_roll5, new = validation_tbl, 
                             type = "response"), 
         pred_WL = ifelse(pred_prob > 0.5, 1, 0))
cm <- confusionMatrix(reference = factor(val_offense_roll5$home_winner_response), 
                      data = factor(val_offense_roll5$pred_WL), positive = "1")
predictions <- prediction(val_offense_roll5$pred_prob, val_offense_roll5$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "offense_roll5",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_offense_roll5$pred_WL),
                                                         reference = factor(val_offense_roll5$home_winner_response),
                                                         positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list(model_offense_roll5))
cmList <- c(cmList, list(cm))

# ESPN Prediction Accuracy
val_espn <- validation_tbl %>%
  mutate(espn_pred_WL = ifelse(home_espn_probability > 50,
                               1, 0))
cm <- confusionMatrix(reference = factor(val_espn$home_winner_response),
                      data = factor(val_espn$espn_pred_WL),
                      positive = "1")
predictions <- prediction(val_espn$home_espn_probability/100, val_espn$home_winner_response)
roc <- performance(predictions,"tpr", "fpr")
auc_ROCR <- performance(predictions, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
modelComp <- modelComp %>% add_row(type = "espn_pred",
                                   accuracy = cm$overall['Accuracy'],
                                   precision = precision(data = factor(val_espn$espn_pred_WL),
                                                         reference = factor(val_espn$home_winner_response),
                                                         positive = "1"),
                                   specificity = cm$byClass['Sensitivity'],
                                   "AUC" = auc_ROCR)
modelList <- c(modelList, list("NOT A MODEL"))
cmList <- c(cmList, list(cm))
rownames(modelComp) <- NULL

view(modelComp)