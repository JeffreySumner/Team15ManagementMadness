if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('caret')) install.packages('caret')
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
  

# Load the glmnet package
library(glmnet)

# Extract the predictor columns from the training data
predictor_cols <- names(train_tbl)[!names(train_tbl) %in% c("home_winner_response")]

# Fit a lasso model to the training data
lasso_model <- cv.glmnet(
  x = as.matrix(train_tbl[, predictor_cols]), 
  y = train_tbl$home_winner_response, 
  family = "binomial", 
  alpha = 1, 
  nfolds = 10
)

# Extract the coefficients using the lambda value that minimizes cross-validation error
coef_vals <- coef(lasso_model, s = "lambda.min")

# Identify the predictors with non-zero coefficients
selected_predictors <- rownames(coef_vals)[-1]

# Convert the coefficients to a matrix and create a data frame
coef_matrix <- as.matrix(coef_vals)
coef_df <- data.frame(
  variable = selected_predictors,
  coefficient = abs(coef_matrix[-1, 1])
)

# Select the top 15 variables with the largest coefficient magnitudes
selected_predictors <- coef_df[order(coef_df$coefficient, decreasing = TRUE), "variable"][1:15]

# Define parameter grid for tuning
param_grid <- expand.grid(C = c(0.1, 1, 10, 100), 
                          sigma = c(0.1, 1, 10, 100),
                          type = c("C-classification", "nu-classification"))

# Create training control object for cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Train and tune SVM model using radial basis kernel
set.seed(123)
tuned_svm_model <- train(
  home_winner_response ~ as.matrix(train_tbl[, selected_predictors, drop=FALSE]), 
  data = train_tbl,
  method = "svmRadial",
  tuneGrid = expand.grid(C = c(0.1, 1, 10, 100), 
                         sigma = c(0.1, 1, 10, 100)),
  trControl = train_control
)



# Get the best SVM model based on cross-validation accuracy
best_svm_model <- tuned_svm_model$best.model

# Make predictions on the test set using the best SVM model
test_preds <- predict(best_svm_model, newdata = test_tbl, type = "class")


























library(e1071)

# Define the parameter grid for tuning
svm_grid <- expand.grid(
  cost = c(0.1, 1, 10, 100),
  gamma = c(0.1, 1, 10, 100)
)

# Fit an SVM model on the reduced dataset using the selected predictors
svm_model <- svm(
  x = as.matrix(train_tbl[, selected_predictors]),
  y = train_tbl$home_winner_response,
  kernel = "linear",
  probability = TRUE
)

# Tune the SVM model using the parameter grid and 5-fold cross-validation
svm_tuned <- tune(
  svm,
  train.x = as.matrix(train_tbl[, selected_predictors]),
  train.y = train_tbl$home_winner_response,
  kernel = "linear",
  max_iter = 50,
  ranges = list(cost = svm_grid$cost, gamma = svm_grid$gamma),
  tunecontrol = tune.control(sampling = "cross", cross = 5)
)

# Extract the best hyperparameters from the tuned model
best_cost <- svm_tuned$best.parameters$cost
best_gamma <- svm_tuned$best.parameters$gamma

# Fit a new SVM model using the best hyperparameters
svm_final <- svm(
  x = as.matrix(train_tbl[, selected_predictors]),
  y = train_tbl$home_winner_response,
  kernel = "linear",
  probability = TRUE,
  cost = best_cost,
  gamma = best_gamma
)

# Make predictions on the test set using the final SVM model
test_preds <- predict(svm_final, newdata = as.matrix(test_predictors), probability = TRUE)







# Fit an SVM model on the reduced dataset using the selected predictors
# svm_model <- svm(
#   x = as.matrix(train_tbl[, selected_predictors]),
#   y = train_tbl$home_winner_response,
#   kernel = "linear",
#   probability = TRUE
# )

# Extract the predictor columns from the test data
test_predictors <- test_tbl[, selected_predictors]

# Make predictions on the test set using the SVM model
test_preds <- predict(svm_model, newdata = as.matrix(test_predictors), probability = TRUE)

test_preds

# Create a confusion matrix to evaluate performance
confusion_matrix <- table(test_tbl$home_winner_response, test_preds)

# Compute accuracy, precision, specificity, and AUC
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
library(pROC)
auc <- auc(roc(test_tbl$home_winner_response, test_preds[,2]))

accuracy
precision
specificity
auc
confusion_matrix


library(vip)
vip(svm_model)



