if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
setwd("C:/Users/mattg/OneDrive/Documents/Team-15/Data/clean")
model_data_tbl <- readr::read_csv("model_data_tbl.csv")

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
    , -awayTeam_id
    , -homeTeam_id
    , -contains("city")
    , -contains("state")
    , -contains("arena")
    , -contains("_lat")
    , -contains("_lon")
    , -away_ap_rank
    , -home_ap_rank
    , -ends_with("_")
    , -home_winner
    , -contains("opp")
    , -away_ap_rank_fct
    , -home_ap_rank_fct
    , -contains("espn_prob")
  )

model_data_tbl <- model_data_tbl %>%
  select(!contains("roll5"))

set.seed(15) # for reproducibility
initial_split_data <- initial_split(model_data_tbl, prop = .6)
train_tbl <- training(initial_split_data)
test_temp_tbl <- testing(initial_split_data)

set.seed(15)
validation_split <- initial_split(test_temp_tbl, prop = .5)
test_tbl <- training(validation_split)
validation_tbl <- testing(validation_split)

rm(initial_split_data, validation_split, test_temp_tbl) # run as needed

#cor(train_tbl)

#train_tbl <- train_tbl %>%
#  select(!contains("roll5")) %>% 
#  select(!contains("arena")) %>%
#  select(!contains("city")) %>%
#  select(!contains("_lat")) %>%
#  select(!contains("_lon")) %>%
#  select(!contains("Team_id")) %>%
#  select(!contains("state")) %>%
#  select(!contains("rank")) %>%
#  select(-contains("opp")) %>%
#  select(-home_winner)



# Load the glmnet package
library(glmnet)

# Load the MASS package
library(MASS)

# Extract the predictor columns from the training data
predictor_cols <- names(train_tbl)[!names(train_tbl) %in% c("home_winner_response")]

# Fit a stepwise logistic regression model on the training data
step_model <- stepAIC(glm(
  formula = home_winner_response ~ .,
  data = train_tbl,
  family = binomial()
), direction = "both", trace = FALSE)

# Identify the predictors with non-zero coefficients
selected_predictors <- names(step_model$coefficients)[!names(step_model$coefficients) %in% "(Intercept)"]


# Fit a logistic regression model on the reduced dataset using the selected predictors
logit_model <- glm(
  formula = paste("home_winner_response ~", paste(selected_predictors, collapse = " + ")), 
  data = train_tbl, 
  family = binomial()
)


# Extract the predictor columns from the test data
test_predictors <- test_tbl[, predictor_cols]

# Make predictions on the test set using the logistic regression model
test_preds <- predict(logit_model, newdata = test_predictors, type = "response")

# Create a confusion matrix to evaluate performance
confusion_matrix <- table(test_tbl$home_winner_response, test_preds > 0.5)

# Compute accuracy, precision, specificity, and AUC
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
library(pROC)
auc <- auc(roc(test_tbl$home_winner_response, test_preds))

accuracy
precision
specificity
auc
confusion_matrix

library(vip)
vip(logit_model)





