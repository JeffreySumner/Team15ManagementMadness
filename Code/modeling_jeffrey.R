if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('vip')) install.packages('vip')

# XGBoost Models ----
## training data condensed ----
train_season_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("season")
         )

train_rolling_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("roll")
  )

train_standard_tbl <- train_tbl %>%
  select(home_winner_response
         , ends_with("_")
  )


## training models ----

boosted_tree <- boost_tree(
  trees = 10
  , tree_depth = tune()
  , min_n = tune()
  , loss_reduction = tune()
  , sample_size = tune()
  , mtry = tune()
  , learn_rate = tune() 
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

## Grid Search ----

xgb_grid_season <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_season_tbl),
  learn_rate(),
  size = 30
)


xgb_grid_rolling <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_rolling_tbl),
  learn_rate(),
  size = 30
)

xgb_grid_standard <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_standard_tbl),
  learn_rate(),
  size = 30
)
## Recipes ----

xgb_season_recipe <- recipe(home_winner_response ~ . , data = train_season_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

xgb_rolling_recipe <- recipe(home_winner_response ~ . , data = train_rolling_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

xgb_standard_recipe <- recipe(home_winner_response ~ . , data = train_standard_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

## Workflows ----
xgb_season_workflow <- workflow() %>% 
  add_recipe(xgb_season_recipe) %>%
  add_model(boosted_tree)

xgb_rolling_workflow <- workflow() %>% 
  add_recipe(xgb_rolling_recipe) %>%
  add_model(boosted_tree)

xgb_standard_workflow <- workflow() %>% 
  add_recipe(xgb_standard_recipe) %>%
  add_model(boosted_tree)

## cv ----

set.seed(15)
xgb_season_fold <- vfold_cv(train_season_tbl, strata = home_winner_response)

set.seed(15)
xgb_rolling_fold <- vfold_cv(train_rolling_tbl, strata = home_winner_response)

set.seed(15)
xgb_standard_fold <- vfold_cv(train_standard_tbl, strata = home_winner_response)

## Create XGB Model Season ----
doParallel::registerDoParallel()
set.seed(15)
xgb_season_res <- tune_grid(
  xgb_season_workflow
  , resamples = xgb_season_fold
  , grid = xgb_grid_season
  , control = control_grid(save_pred = TRUE)
)

xgb_season_res

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

fitted_xgb_best_season <- fit(final_xgb_season, train_season_tbl)
xgb_season_test_pred <- predict(fitted_xgb_best_season, test_tbl, type = "prob")
xgb_season_valid_pred <- predict(fitted_xgb_best_season, validation_tbl, type = "prob")


## Create XGB Model Rolling ----
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

fitted_xgb_best_rolling <- fit(final_xgb_rolling, train_rolling_tbl)
xgb_rolling_test_pred <- predict(fitted_xgb_best_rolling, test_tbl, type = "prob")
xgb_rolling_valid_pred <- predict(fitted_xgb_best_rolling, validation_tbl, type = "prob")

## Create XGB Model Standard ----
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

fitted_xgb_best_standard <- fit(final_xgb_standard, train_standard_tbl)
xgb_standard_test_pred <- predict(fitted_xgb_best_standard, test_tbl, type = "prob")
xgb_standard_valid_pred <- predict(fitted_xgb_best_standard, validation_tbl, type = "prob")


# GLM Logistic Reg Models ----
## training data condensed ----
train_season_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("season")
  )

train_rolling_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("roll")
  )

train_standard_tbl <- train_tbl %>%
  select(home_winner_response
         , ends_with("_")
  )


## training models ----

glm_log <- logistic_reg(
  penalty = tune()
  , mixture = 1
  ) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

## Grid Search ----

glm_log_grid_season <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

glm_log_grid_rolling <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

glm_log_grid_standard <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

## Recipes ----

glm_log_season_recipe <- recipe(home_winner_response ~ . , data = train_season_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

glm_log_rolling_recipe <- recipe(home_winner_response ~ . , data = train_rolling_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

glm_log_standard_recipe <- recipe(home_winner_response ~ . , data = train_standard_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

## Workflows ----
glm_log_season_workflow <- workflow() %>% 
  add_recipe(glm_log_season_recipe) %>%
  add_model(glm_log)

glm_log_rolling_workflow <- workflow() %>% 
  add_recipe(glm_log_rolling_recipe) %>%
  add_model(glm_log)

glm_log_standard_workflow <- workflow() %>% 
  add_recipe(glm_log_standard_recipe) %>%
  add_model(glm_log)

## cv ----

set.seed(15)
glm_log_season_fold <- vfold_cv(train_season_tbl, strata = home_winner_response)

set.seed(15)
glm_log_rolling_fold <- vfold_cv(train_rolling_tbl, strata = home_winner_response)

set.seed(15)
glm_log_standard_fold <- vfold_cv(train_standard_tbl, strata = home_winner_response)

## Create glm_log Model Season ----
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

fitted_glm_log_best_season <- fit(final_glm_log_season, train_season_tbl)
glm_log_season_test_pred <- predict(fitted_glm_log_best_season, test_tbl, type = "prob")
glm_log_season_valid_pred <- predict(fitted_glm_log_best_season, validation_tbl, type = "prob")

## Create glm_log Model Rolling ----
doParallel::registerDoParallel()
set.seed(15)
glm_log_rolling_res <- tune_grid(
  glm_log_rolling_workflow
  , resamples = glm_log_rolling_fold
  , grid = glm_log_grid_rolling
  , control = control_grid(save_pred = TRUE)
  , metrics = metric_set(roc_auc)
)

glm_log_rolling_res

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

fitted_glm_log_best_rolling <- fit(final_glm_log_rolling, train_rolling_tbl)
glm_log_rolling_test_pred <- predict(fitted_glm_log_best_rolling, test_tbl, type = "prob")
glm_log_rolling_valid_pred <- predict(fitted_glm_log_best_rolling, validation_tbl, type = "prob")

## Create glm_log Model Standard ----
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

fitted_glm_log_best_standard <- fit(final_glm_log_standard, train_standard_tbl)
glm_log_standard_test_pred <- predict(fitted_glm_log_best_standard, test_tbl, type = "prob") 
glm_log_standard_valid_pred <- predict(fitted_glm_log_best_standard, validation_tbl, type = "prob") 



# SVM Models ----
## training data condensed ----
train_season_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("season")
  )

train_rolling_tbl <- train_tbl %>%
  select(home_winner_response
         , contains("roll")
  )

train_standard_tbl <- train_tbl %>%
  select(home_winner_response
         , ends_with("_")
  )


## training models ----

svm_kernlab <- svm_linear() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

## Grid Search ----

glm_log_grid_season <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

glm_log_grid_rolling <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

glm_log_grid_standard <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

## Recipes ----

glm_log_season_recipe <- recipe(home_winner_response ~ . , data = train_season_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

glm_log_rolling_recipe <- recipe(home_winner_response ~ . , data = train_rolling_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

glm_log_standard_recipe <- recipe(home_winner_response ~ . , data = train_standard_tbl) %>%
  step_dummy(all_nominal_predictors()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

## Workflows ----
glm_log_season_workflow <- workflow() %>% 
  add_recipe(glm_log_season_recipe) %>%
  add_model(glm_log)

glm_log_rolling_workflow <- workflow() %>% 
  add_recipe(glm_log_rolling_recipe) %>%
  add_model(glm_log)

glm_log_standard_workflow <- workflow() %>% 
  add_recipe(glm_log_standard_recipe) %>%
  add_model(glm_log)

## cv ----

set.seed(15)
glm_log_season_fold <- vfold_cv(train_season_tbl, strata = home_winner_response)

set.seed(15)
glm_log_rolling_fold <- vfold_cv(train_rolling_tbl, strata = home_winner_response)

set.seed(15)
glm_log_standard_fold <- vfold_cv(train_standard_tbl, strata = home_winner_response)

## Create glm_log Model Season ----
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

fitted_glm_log_best_season <- fit(final_glm_log_season, train_season_tbl)
glm_log_season_test_pred <- predict(fitted_glm_log_best_season, test_tbl, type = "prob")
glm_log_season_valid_pred <- predict(fitted_glm_log_best_season, validation_tbl, type = "prob")

## Create glm_log Model Rolling ----
doParallel::registerDoParallel()
set.seed(15)
glm_log_rolling_res <- tune_grid(
  glm_log_rolling_workflow
  , resamples = glm_log_rolling_fold
  , grid = glm_log_grid_rolling
  , control = control_grid(save_pred = TRUE)
  , metrics = metric_set(roc_auc)
)

glm_log_rolling_res

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

fitted_glm_log_best_rolling <- fit(final_glm_log_rolling, train_rolling_tbl)
glm_log_rolling_test_pred <- predict(fitted_glm_log_best_rolling, test_tbl, type = "prob")
glm_log_rolling_valid_pred <- predict(fitted_glm_log_best_rolling, validation_tbl, type = "prob")

## Create glm_log Model Standard ----
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

fitted_glm_log_best_standard <- fit(final_glm_log_standard, train_standard_tbl)
glm_log_standard_test_pred <- predict(fitted_glm_log_best_standard, test_tbl, type = "prob") 
glm_log_standard_valid_pred <- predict(fitted_glm_log_best_standard, validation_tbl, type = "prob") 




# Model Comparison ----

all_models_tbl <- bind_rows(
  glm_log_standard_test_pred %>% mutate(model_type = "glm_log", col_type = "standard", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response)
  , glm_log_standard_valid_pred  %>% mutate(model_type = "glm_log", col_type = "standard", data_type = "valid", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response)
  , xgb_standard_test_pred %>% mutate(model_type = "xgb", col_type = "standard", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response)
  , xgb_standard_valid_pred %>% mutate(model_type = "xgb", col_type = "standard", data_type = "valid", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response)
  
  , glm_log_season_test_pred %>% mutate(model_type = "glm_log", col_type = "season", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response)
  , glm_log_season_valid_pred %>% mutate(model_type = "glm_log", col_type = "season", data_type = "valid", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response)
  , xgb_season_test_pred %>% mutate(model_type = "xgb", col_type = "season", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response)
  , xgb_season_valid_pred %>% mutate(model_type = "xgb", col_type = "season", data_type = "valid", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response)
  
  , glm_log_rolling_test_pred %>% mutate(model_type = "glm_log", col_type = "rolling", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response)
  , glm_log_rolling_valid_pred %>% mutate(model_type = "glm_log", col_type = "rolling", data_type = "valid", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response)
  , xgb_rolling_test_pred %>% mutate(model_type = "xgb", col_type = "rolling", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response)
  , xgb_rolling_valid_pred %>% mutate(model_type = "xgb", col_type = "rolling", data_type = "valid", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response)
)

confusion_matrix_tbl <- all_models_tbl %>%
  mutate(pred = ifelse(.pred_1 >= .5,1,0)
         , espn_prob = ifelse(espn_prob >= 50,1,0)
         , winner = winner %>% as.numeric() -1
  ) %>%
  select(3:ncol(.)) %>%
  pivot_longer(cols = c(espn_prob,pred), names_to = "name",values_to = "values") %>%
  group_by(model_type,col_type,data_type, name) %>%
  summarize(confusion_matrix = list(table(winner,values))
            , specificity = caret::specificity(confusion_matrix[[1]])
            , sensitivity = caret::specificity(confusion_matrix[[1]])
            , accuracy = yardstick::accuracy(confusion_matrix[[1]])
  )


readr::write_rds(confusion_matrix_tbl, "Data/clean/confusion_matrix_jeffrey.rds")
