# Required Packages 
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('glmnet')) install.packages('glmnet')

# Step 1: Split Data into Train, Test and Validation ----
## Read in Data ----
model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")

model_data_tbl <- model_data_tbl %>%
  mutate(home_winner_response = as.numeric(home_winner) %>% factor()
         , away_ap_rank_fct = factor(away_ap_rank_fct)
         , home_ap_rank_fct = factor(home_ap_rank_fct)
         , homeTeam_id = factor(homeTeam_id)
         , awayTeam_id = factor(awayTeam_id)
  )  %>%
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
  ) %>% mutate(id = row_number())

## Split Data ----
set.seed(15) # for reproducibility
initial_split_data <- initial_split(model_data_tbl, prop = .6)
train_tbl <- training(initial_split_data)
test_temp_tbl <- testing(initial_split_data)

set.seed(15)
validation_split <- initial_split(test_temp_tbl, prop = .5)
test_tbl <- training(validation_split)
validation_tbl <- testing(validation_split)


## Create Season, Rolling and Standard Training
## training data condensed ----
train_season_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("season")
         , -id
  )

train_rolling_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("roll")
         , -id
  )

train_standard_tbl <- train_tbl %>%
  select(home_winner_response
         , ends_with("_")
         , -id
         , -away_points_
         , -away_points_opp_
         , -home_points_
         , -home_points_opp_
  )

# Step 2: Perform Lasso Regression to select influential parameters ----

season_predictor_cols <- names(train_season_tbl)[!names(train_season_tbl) %in% c("home_winner_response")]
rolling_predictor_cols <- names(train_rolling_tbl)[!names(train_rolling_tbl) %in% c("home_winner_response")]
standard_predictor_cols <- names(train_standard_tbl)[!names(train_standard_tbl) %in% c("home_winner_response")]


## Fit a lasso model to each training tibble ----
season_lasso_model <- cv.glmnet(
  x = as.matrix(train_season_tbl[, season_predictor_cols]), 
  y = train_season_tbl$home_winner_response, 
  family = "binomial", 
  alpha = 1, 
  nfolds = 10
)

rolling_lasso_model <- cv.glmnet(
  x = as.matrix(train_rolling_tbl[, rolling_predictor_cols]), 
  y = train_rolling_tbl$home_winner_response, 
  family = "binomial", 
  alpha = 1, 
  nfolds = 10
)

standard_lasso_model <- cv.glmnet(
  x = as.matrix(train_standard_tbl[, standard_predictor_cols]), 
  y = train_standard_tbl$home_winner_response, 
  family = "binomial", 
  alpha = 1, 
  nfolds = 10
)

plot(standard_lasso_model) 

## Determine Influential coefficients ----
### Season ----
season_coef_vals <- coef(season_lasso_model, s = "lambda.min")
season_coef_vals <- as.matrix(season_coef_vals)

# Identify the predictors with non-zero coefficients
# selected_predictors <- rownames(coef_vals)[-1]

# There is a major difference between the logical operator of "!=0! and ">0", ">0" cuts out more
season_selected_predictors <- rownames(season_coef_vals)[season_coef_vals!=0]

### Rolling ----
rolling_coef_vals <- coef(rolling_lasso_model, s = "lambda.min")
rolling_coef_vals <- as.matrix(rolling_coef_vals)

# Identify the predictors with non-zero coefficients
# selected_predictors <- rownames(coef_vals)[-1]

# There is a major difference between the logical operator of "!=0! and ">0", ">0" cuts out more
rolling_selected_predictors <- rownames(rolling_coef_vals)[rolling_coef_vals!=0]

### Standard ----
standard_coef_vals <- coef(standard_lasso_model, s = "lambda.min")
standard_coef_vals <- as.matrix(standard_coef_vals)

# Identify the predictors with non-zero coefficients
# selected_predictors <- rownames(coef_vals)[-1]

# There is a major difference between the logical operator of "!=0! and ">0", ">0" cuts out more
standard_selected_predictors <- rownames(standard_coef_vals)[standard_coef_vals!=0]

# Step 3: Create Models with Lasso Influential coefficients ----
## Condense training data once more ----
train_season_lasso_tbl <- train_season_tbl %>%
  select(home_winner_response
         , any_of(season_selected_predictors)
  )

train_rolling_lasso_tbl <- train_rolling_tbl %>%
  select(home_winner_response
         , any_of(rolling_selected_predictors)
  )

train_standard_lasso_tbl <- train_standard_tbl %>%
  select(home_winner_response
         , any_of(standard_selected_predictors) 
  )

## Fit Models ----
### Simple Logit ----
#### Season Model ----
simple_season_logit_model <- glm(
  formula = home_winner_response ~ ., 
  data = train_season_lasso_tbl, 
  family = binomial(link = 'logit')
)
readr::write_rds(simple_season_logit_model, "Data/models/simple_season_logit_model.rds")
#### Rolling Model ----
simple_rolling_logit_model <- glm(
  formula = home_winner_response ~ ., 
  data = train_rolling_lasso_tbl, 
  family = binomial(link = 'logit')
)
readr::write_rds(simple_rolling_logit_model, "Data/models/simple_rolling_logit_model.rds")
#### Standard Model ----
simple_standard_logit_model <- glm(
  formula = home_winner_response ~ ., 
  data = train_standard_lasso_tbl, 
  family = binomial(link = 'logit')
)
readr::write_rds(simple_standard_logit_model, "Data/models/simple_standard_logit_model.rds")

### Simple Probit ----
#### Season Model ----
simple_season_probit_model <- glm(
  formula = home_winner_response ~ ., 
  data = train_season_lasso_tbl, 
  family = binomial(link = 'probit')
)
readr::write_rds(simple_season_probit_model, "Data/models/simple_season_probit_model.rds")
#### Rolling Model ----
simple_rolling_probit_model <- glm(
  formula = home_winner_response ~ ., 
  data = train_rolling_lasso_tbl, 
  family = binomial(link = 'probit')
)
readr::write_rds(simple_rolling_probit_model, "Data/models/simple_rolling_probit_model.rds")
#### Standard Model ----
simple_standard_probit_model <- glm(
  formula = home_winner_response ~ ., 
  data = train_standard_lasso_tbl, 
  family = binomial(link = 'probit')
)
readr::write_rds(simple_standard_probit_model, "Data/models/simple_standard_probit_model.rds")

### Enhanced Logit ----
#### Tidymodel Initiation----

glm_log <- logistic_reg(
  penalty = tune()
  , mixture = 1
) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

#### Grid Search for cross validation ----

glm_log_grid_season <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

glm_log_grid_rolling <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

glm_log_grid_standard <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

#### Normalize & PCA ----

glm_log_season_recipe <- recipe(home_winner_response ~ . , data = train_season_lasso_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

glm_log_rolling_recipe <- recipe(home_winner_response ~ . , data = train_rolling_lasso_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

glm_log_standard_recipe <- recipe(home_winner_response ~ . , data = train_standard_lasso_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

#### Create Workflows ----
glm_log_season_workflow <- workflow() %>% 
  add_recipe(glm_log_season_recipe) %>%
  add_model(glm_log)

glm_log_rolling_workflow <- workflow() %>% 
  add_recipe(glm_log_rolling_recipe) %>%
  add_model(glm_log)

glm_log_standard_workflow <- workflow() %>% 
  add_recipe(glm_log_standard_recipe) %>%
  add_model(glm_log)

#### cv ----

set.seed(15)
glm_log_season_fold <- vfold_cv(train_season_lasso_tbl, strata = home_winner_response)

set.seed(15)
glm_log_rolling_fold <- vfold_cv(train_rolling_lasso_tbl, strata = home_winner_response)

set.seed(15)
glm_log_standard_fold <- vfold_cv(train_standard_lasso_tbl, strata = home_winner_response)

#### Enhanced Logit Season Model ----
doParallel::registerDoParallel()
set.seed(15)
glm_log_season_res <- tune_grid(
  glm_log_season_workflow
  , resamples = glm_log_season_fold
  , grid = glm_log_grid_season
  , control = control_grid(save_pred = TRUE)
  , metrics = metric_set(roc_auc)
)

glm_log_season_res

glm_log_season_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

best_auc_glm_log_season <- select_best(glm_log_season_res, "roc_auc")
best_auc_glm_log_season

final_glm_log_season <- finalize_workflow(
  glm_log_season_workflow,
  best_auc_glm_log_season
)

final_glm_log_season

final_glm_log_season %>%
  fit(data = train_season_tbl) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_glm_log_season <- last_fit(final_glm_log_season, initial_split_data)

collect_metrics(final_res_glm_log_season)

fitted_glm_log_best_season <- fit(final_glm_log_season, train_season_lasso_tbl)
# glm_log_season_test_pred <- predict(fitted_glm_log_best_season, test_tbl, type = "prob")
# glm_log_season_valid_pred <- predict(fitted_glm_log_best_season, validation_tbl, type = "prob")

#### Enhanced Logit Rolling Model----
doParallel::registerDoParallel()
set.seed(15)
glm_log_rolling_res <- tune_grid(
  glm_log_rolling_workflow
  , resamples = glm_log_rolling_fold
  , grid = glm_log_grid_rolling
  , control = control_grid(save_pred = TRUE)
  , metrics = metric_set(roc_auc)
)

# glm_log_rolling_res

glm_log_rolling_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

best_auc_glm_log_rolling <- select_best(glm_log_rolling_res, "roc_auc")
best_auc_glm_log_rolling

final_glm_log_rolling <- finalize_workflow(
  glm_log_rolling_workflow,
  best_auc_glm_log_rolling
)

final_glm_log_rolling

final_glm_log_rolling %>%
  fit(data = train_rolling_tbl) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_glm_log_rolling <- last_fit(final_glm_log_rolling, initial_split_data)

collect_metrics(final_res_glm_log_rolling)

fitted_glm_log_best_rolling <- fit(final_glm_log_rolling, train_rolling_lasso_tbl)
# glm_log_rolling_test_pred <- predict(fitted_glm_log_best_rolling, test_tbl, type = "prob")
# glm_log_rolling_valid_pred <- predict(fitted_glm_log_best_rolling, validation_tbl, type = "prob")

#### Enhanced Logit Standard Model ----
doParallel::registerDoParallel()
set.seed(15)
glm_log_standard_res <- tune_grid(
  glm_log_standard_workflow
  , resamples = glm_log_standard_fold
  , grid = glm_log_grid_standard
  , control = control_grid(save_pred = TRUE)
  , metrics = metric_set(roc_auc)
)

glm_log_standard_res

glm_log_standard_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

best_auc_glm_log_standard <- select_best(glm_log_standard_res, "roc_auc")
best_auc_glm_log_standard

final_glm_log_standard <- finalize_workflow(
  glm_log_standard_workflow,
  best_auc_glm_log_standard
)

final_glm_log_standard

final_glm_log_standard %>%
  fit(data = train_standard_tbl) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_glm_log_standard <- last_fit(final_glm_log_standard, initial_split_data)

collect_metrics(final_res_glm_log_standard)

# fitted_glm_log_best_standard <- fit(final_glm_log_standard, train_standard_tbl)
# glm_log_standard_test_pred <- predict(fitted_glm_log_best_standard, test_tbl, type = "prob") 
# glm_log_standard_valid_pred <- predict(fitted_glm_log_best_standard, validation_tbl, type = "prob") 


### Enhanced Decision Trees (XGBOOST) ----
#### Tidymodel Initiation ----

boosted_tree <- boost_tree(
  trees = 100
  , tree_depth = tune()
  , min_n = tune()
  , loss_reduction = tune()
  , sample_size = tune()
  , mtry = tune()
  , learn_rate = tune() 
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

#### Grid Search for cross validation ----

xgb_grid_season <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_season_lasso_tbl),
  learn_rate(),
  size = 30
)


xgb_grid_rolling <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_rolling_lasso_tbl),
  learn_rate(),
  size = 30
)

xgb_grid_standard <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_standard_lasso_tbl),
  learn_rate(),
  size = 30
)
#### Normalize & PCA ----

xgb_season_recipe <- recipe(home_winner_response ~ . , data = train_season_lasso_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

xgb_rolling_recipe <- recipe(home_winner_response ~ . , data = train_rolling_lasso_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

xgb_standard_recipe <- recipe(home_winner_response ~ . , data = train_standard_lasso_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

#### Create Workflows ----
xgb_season_workflow <- workflow() %>% 
  add_recipe(xgb_season_recipe) %>%
  add_model(boosted_tree)

xgb_rolling_workflow <- workflow() %>% 
  add_recipe(xgb_rolling_recipe) %>%
  add_model(boosted_tree)

xgb_standard_workflow <- workflow() %>% 
  add_recipe(xgb_standard_recipe) %>%
  add_model(boosted_tree)

#### cv ----

set.seed(15)
xgb_season_fold <- vfold_cv(train_season_lasso_tbl, strata = home_winner_response)

set.seed(15)
xgb_rolling_fold <- vfold_cv(train_rolling_lasso_tbl, strata = home_winner_response)

set.seed(15)
xgb_standard_fold <- vfold_cv(train_standard_lasso_tbl, strata = home_winner_response)

#### Enhanced DT Season Model ----
doParallel::registerDoParallel()
set.seed(15)
xgb_season_res <- tune_grid(
  xgb_season_workflow
  , resamples = xgb_season_fold
  , grid = xgb_grid_season
  , control = control_grid(save_pred = TRUE)
)

# xgb_season_res

xgb_season_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


best_auc_xgb_season <- select_best(xgb_season_res, "roc_auc")
best_auc_xgb_season

final_xgb_season <- finalize_workflow(
  xgb_season_workflow,
  best_auc_xgb_season
)

final_xgb_season

final_xgb_season %>%
  fit(data = train_season_tbl) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_xgb_season <- last_fit(final_xgb_season, initial_split_data)

collect_metrics(final_res_xgb_season)

fitted_xgb_best_season <- fit(final_xgb_season, train_season_lasso_tbl)
# xgb_season_test_pred <- predict(fitted_xgb_best_season, test_tbl, type = "prob")
# xgb_season_valid_pred <- predict(fitted_xgb_best_season, validation_tbl, type = "prob")


#### Enhanced DT Rolling Model ----
doParallel::registerDoParallel()
set.seed(15)
xgb_rolling_res <- tune_grid(
  xgb_rolling_workflow
  , resamples = xgb_rolling_fold
  , grid = xgb_grid_rolling
  , control = control_grid(save_pred = TRUE)
)

xgb_rolling_res

xgb_rolling_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


best_auc_xgb_rolling <- select_best(xgb_rolling_res, "roc_auc")
best_auc_xgb_rolling

final_xgb_rolling <- finalize_workflow(
  xgb_rolling_workflow,
  best_auc_xgb_rolling
)

final_xgb_rolling

final_xgb_rolling %>%
  fit(data = train_rolling_tbl) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_xgb_rolling <- last_fit(final_xgb_rolling, initial_split_data)

collect_metrics(final_res_xgb_rolling)

fitted_xgb_best_rolling <- fit(final_xgb_rolling, train_rolling_lasso_tbl)
# xgb_rolling_test_pred <- predict(fitted_xgb_best_rolling, test_tbl, type = "prob")
# xgb_rolling_valid_pred <- predict(fitted_xgb_best_rolling, validation_tbl, type = "prob")

#### Enhanced DT Standard Model ----
doParallel::registerDoParallel()
set.seed(15)
xgb_standard_res <- tune_grid(
  xgb_standard_workflow
  , resamples = xgb_standard_fold
  , grid = xgb_grid_standard
  , control = control_grid(save_pred = TRUE)
)

xgb_standard_res

xgb_standard_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


best_auc_xgb_standard <- select_best(xgb_standard_res, "roc_auc")
best_auc_xgb_standard

final_xgb_standard <- finalize_workflow(
  xgb_standard_workflow,
  best_auc_xgb_standard
)

final_xgb_standard

final_xgb_standard %>%
  fit(data = train_standard_tbl) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_xgb_standard <- last_fit(final_xgb_standard, initial_split_data)

collect_metrics(final_res_xgb_standard)

fitted_xgb_best_standard <- fit(final_xgb_standard, train_standard_lasso_tbl)
# xgb_standard_test_pred <- predict(fitted_xgb_best_standard, test_tbl, type = "prob")
# xgb_standard_valid_pred <- predict(fitted_xgb_best_standard, validation_tbl, type = "prob")
