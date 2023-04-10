lasso_season_logit_model <- readr::read_rds("Data/clean/lasso_season_logit_model.rds")
lasso_season_model <- readr::read_rds("Data/clean/lasso_season_model.rds")
logit_roll5_model <- readr::read_rds("Data/clean/logit_roll5_model.rds")
step_season_logit_model <- readr::read_rds("Data/clean/step_season_logit_model.rds")
step_season_model <- readr::read_rds("Data/clean/step_season_model.rds")

lasso_season_logit_model_test_preds <- predict(lasso_season_logit_model, newdata = test_tbl, type = "response")
step_season_logit_model_test_preds <- predict(step_season_logit_model, newdata = test_tbl, type = "response")
logit_roll5_model_test_preds <- predict(logit_roll5_model, newdata = test_tbl, type = "response")

table(test_tbl$home_winner_response, lasso_season_logit_model_test_preds > 0.5)
table(test_tbl$home_winner_response, step_season_logit_model_test_preds > 0.5)
table(test_tbl$home_winner_response, logit_roll5_model_test_preds > 0.5)


lasso_season_logit_model_validation_preds <- predict(lasso_season_logit_model, newdata = validation_tbl, type = "response")
step_season_logit_model_validation_preds <- predict(step_season_logit_model, newdata = validation_tbl, type = "response")
logit_roll5_model_validation_preds <- predict(logit_roll5_model, newdata = validation_tbl, type = "response")

table(validation_tbl$home_winner_response, lasso_season_logit_model_validation_preds > 0.5)
table(validation_tbl$home_winner_response, step_season_logit_model_validation_preds > 0.5)
table(validation_tbl$home_winner_response, logit_roll5_model_validation_preds > 0.5)


all_models_tbl <- bind_rows(
  glm_log_standard_test_pred %>%
    select(pred = .pred_1) %>%
    mutate(model_type = "glm_log", col_type = "standard", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , glm_log_standard_valid_pred  %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "glm_log", col_type = "standard", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  , xgb_standard_test_pred %>%
    select(pred = .pred_1) %>%
    mutate(model_type = "xgb", col_type = "standard", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , xgb_standard_valid_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "xgb", col_type = "standard", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  
  , glm_log_season_test_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "glm_log", col_type = "season", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , glm_log_season_valid_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "glm_log", col_type = "season", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  , xgb_season_test_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "xgb", col_type = "season", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , xgb_season_valid_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "xgb", col_type = "season", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  
  , glm_log_rolling_test_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "glm_log", col_type = "rolling", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , glm_log_rolling_valid_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "glm_log", col_type = "rolling", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  , xgb_rolling_test_pred %>% 
    select(pred = .pred_1) %>%
    mutate(model_type = "xgb", col_type = "rolling", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , xgb_rolling_valid_pred %>%
    select(pred = .pred_1) %>%
    mutate(model_type = "xgb", col_type = "rolling", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  
  , lasso_season_logit_model_test_preds %>%
    tibble(pred = .) %>%
    mutate(model_type = "lasso_logit", col_type = "season", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , lasso_season_logit_model_validation_preds %>%
    tibble(pred = .) %>%
    mutate(model_type = "lasso_logit", col_type = "season", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  
  , step_season_logit_model_test_preds %>%
    tibble(pred = .) %>%
    mutate(model_type = "step_logit", col_type = "season", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , step_season_logit_model_validation_preds %>%
    tibble(pred = .) %>%
    mutate(model_type = "step_logit", col_type = "season", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)
  
  , logit_roll5_model_test_preds %>%
    tibble(pred = .) %>%
    mutate(model_type = "logit", col_type = "rolling", data_type = "test", espn_prob = test_tbl$home_espn_probability, winner = test_tbl$home_winner_response, id = test_tbl$id)
  , logit_roll5_model_validation_preds %>%
    tibble(pred = .) %>%
    mutate(model_type = "logit", col_type = "rolling", data_type = "validation", espn_prob = validation_tbl$home_espn_probability, winner = validation_tbl$home_winner_response, id = validation_tbl$id)

)

confusion_matrix_tbl <- all_models_tbl %>%
  select(-id) %>%
  mutate(pred = ifelse(pred >= .5,1,0)
         , espn_prob = ifelse(espn_prob >= 50,1,0)
         , winner = winner %>% as.numeric() -1
  ) %>%
  pivot_longer(cols = c(espn_prob,pred), names_to = "name",values_to = "values") %>%
  group_by(model_type,col_type,data_type, name) %>%
  summarize(confusion_matrix = list(table(winner,values))
            , specificity = caret::specificity(confusion_matrix[[1]])
            , sensitivity = caret::sensitivity(confusion_matrix[[1]])
            , accuracy = yardstick::accuracy(confusion_matrix[[1]])
  )

confusion_matrix_ensemble_tbl <- all_models_tbl %>%
  filter(!model_type %in% "xgb") %>%
  group_by(col_type,data_type, id) %>%
  summarize(pred = mean(pred), espn_prob = mean(espn_prob), winner = mean(winner %>% as.numeric() -1)) %>%
  mutate(pred = ifelse(pred >= .5,1,0)
         , espn_prob = ifelse(espn_prob >= 50,1,0)
  ) %>%
  ungroup() %>%
  select(-id) %>%
  pivot_longer(cols = c(espn_prob,pred), names_to = "name",values_to = "values") %>%
  group_by(col_type,data_type, name) %>%
  summarize(confusion_matrix = list(table(winner,values))
            , specificity = caret::specificity(confusion_matrix[[1]])
            , sensitivity = caret::sensitivity(confusion_matrix[[1]])
            , accuracy = yardstick::accuracy(confusion_matrix[[1]])
  )

confusion_matrix_ensemble_tbl2 <- all_models_tbl %>%
  filter(!model_type %in% "xgb") %>%
  group_by(col_type,data_type, id) %>%
  summarize(pred = mean(pred), espn_prob = mean(espn_prob), winner = mean(winner %>% as.numeric() -1)) %>%
  mutate(pred = ifelse(pred >= .5,1,0)
         , espn_prob = ifelse(espn_prob >= 50,1,0)
         , pred_espn_prob = round((pred+espn_prob)/2)
  ) %>%
  ungroup() %>%
  select(-id) %>%
  pivot_longer(cols = c(espn_prob,pred,pred_espn_prob), names_to = "name",values_to = "values") %>%
  group_by(col_type,data_type, name) %>%
  summarize(confusion_matrix = list(table(winner,values))
            , specificity = caret::specificity(confusion_matrix[[1]])
            , sensitivity = caret::sensitivity(confusion_matrix[[1]])
            , accuracy = yardstick::accuracy(confusion_matrix[[1]])
  )
