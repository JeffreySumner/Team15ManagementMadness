# Required Packages 
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('glmnet')) install.packages('glmnet')
if (!require('vip')) install.packages('vip')
if (!require('caret')) install.packages('caret')
if (!require('here')) install.packages('here')
if (!require('xgboost')) install.packages('xgboost')

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
    game_id
    , contains("home_")
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

standard_season_test_tbl <- test_tbl %>%
  select(home_winner_response
         , contains("season")
         , -id
         , home_distance
         , away_distance
         # , home_ap_rank_fct
         # , away_ap_rank_fct
  )
names(standard_season_test_tbl) <- gsub("season_avg", "", names(standard_season_test_tbl))
  
standard_season_validation_tbl <- validation_tbl %>%
  select(home_winner_response
         , contains("season")
         , -id
         , home_distance
         , away_distance
         # , home_ap_rank_fct
         # , away_ap_rank_fct
  )
names(standard_season_validation_tbl) <- gsub("season_avg", "", names(standard_season_validation_tbl))

standard_rolling_test_tbl <- test_tbl %>%
  select(home_winner_response
         , contains("roll5")
         , -id
         , home_distance
         , away_distance
         # , home_ap_rank_fct
         # , away_ap_rank_fct
  )
names(standard_rolling_test_tbl) <- gsub("roll5", "", names(standard_rolling_test_tbl))

standard_rolling_validation_tbl <- validation_tbl %>%
  select(home_winner_response
         , contains("roll5")
         , -id
         , home_distance
         , away_distance
         # , home_ap_rank_fct
         # , away_ap_rank_fct
  )
names(standard_rolling_validation_tbl) <- gsub("roll5", "", names(standard_rolling_validation_tbl))

# Read in Model Data ----
## Create Season, Rolling and Standard Training
baseline_rolling_logit_model <- readr::read_rds("Data/models/baseline_rolling_logit_model.rds")
baseline_season_logit_model <- readr::read_rds("Data/models/baseline_season_logit_model.rds")
baseline_standard_logit_model <- readr::read_rds("Data/models/baseline_standard_logit_model.rds")
enhanced_rolling_logit_model <- readr::read_rds("Data/models/enhanced_rolling_logit_model.rds")
enhanced_season_logit_model <- readr::read_rds("Data/models/enhanced_season_logit_model.rds")
enhanced_standard_logit_model <- readr::read_rds("Data/models/enhanced_standard_logit_model.rds")
enhanced_rolling_xgb_model <- readr::read_rds("Data/models/enhanced_rolling_xgb_model.rds")
enhanced_season_xgb_model <- readr::read_rds("Data/models/enhanced_season_xgb_model.rds")
enhanced_standard_xgb_model <- readr::read_rds("Data/models/enhanced_standard_xgb_model.rds")
simple_rolling_logit_model <- readr::read_rds("Data/models/simple_rolling_logit_model.rds")
simple_season_logit_model <- readr::read_rds("Data/models/simple_season_logit_model.rds")
simple_standard_logit_model <- readr::read_rds("Data/models/simple_standard_logit_model.rds")
simple_rolling_probit_model <- readr::read_rds("Data/models/simple_rolling_probit_model.rds")
simple_season_probit_model <- readr::read_rds("Data/models/simple_season_probit_model.rds")
simple_standard_probit_model <- readr::read_rds("Data/models/simple_standard_probit_model.rds")

# Create Model Prediction Tibble ----
create_tibble <- function(x){
  
  return(tibble(name = x))
  
}

model_tbl <- lapply(ls(), create_tibble) %>% 
  bind_rows() %>% 
  rowwise() %>% 
  mutate(model = list(get(name))) %>%
  filter(stringr::str_detect(name,"_model")) %>%
  mutate(
    pred_test = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, test_tbl, type = 'prob') %>% pull(2) %>% round()), list(predict(model, test_tbl, type = 'response') %>% as.vector() %>% round() ))
    , pred_validation = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, validation_tbl, type = 'prob') %>% pull(2) %>% round() ), list(predict(model, validation_tbl, type = 'response') %>% as.vector() %>% round() ))
    , response_test = list(test_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
    , response_validation = list(validation_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
    , test_confusion_matrix = list(caret::confusionMatrix(reference = response_test %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                          , data=pred_test %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                          )
                                   )
    , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                                , data=pred_validation %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                                )
                                         )
  )  %>%
  ungroup() %>%
  select(-model)  %>%
  separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE) %>%
  filter(!Metric %in% "standard")
# model_standard_season_tbl <- lapply(ls(), create_tibble) %>% 
#   bind_rows() %>% 
#   rowwise() %>% 
#   mutate(model = list(get(name))) %>%
#   filter(stringr::str_detect(name,"_model")) %>%
#   separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE) %>%
#   filter(Metric %in% "standard") %>%
#   rowwise() %>% 
#   # select(-model) %>%
#   # mutate(check = stringr::str_detect(Simplicity,"enhanced"))
#   mutate(
#     pred_test = ifelse(stringr::str_detect(Simplicity,"enhanced")
#                        , list(predict(model, standard_season_test_tbl, type = 'prob') %>% pull(2) %>% round())
#                        , list(predict(model, standard_season_test_tbl, type = 'response') %>% as.vector() %>% round() )
#                        )
#     
#     , pred_validation = ifelse(stringr::str_detect(Simplicity,"enhanced"), list(predict(model, standard_season_validation_tbl, type = 'prob') %>% pull(2) %>% round() ), list(predict(model, standard_season_validation_tbl, type = 'response') %>% as.vector() %>% round() ))
#     , response_test = list(standard_season_test_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
#     , response_validation = list(standard_season_validation_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
#     , test_confusion_matrix = list(caret::confusionMatrix(reference = response_test %>% factor(), data=pred_test %>% factor()))
#     , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation %>% factor(), data=pred_validation %>% factor()))
#   )  %>%
#   ungroup() %>%
#   select(-model) %>%
#   mutate(Metric = "standard-season")
# 
# model_standard_rolling_tbl <- lapply(ls(), create_tibble) %>% 
#   bind_rows() %>% 
#   rowwise() %>% 
#   mutate(model = list(get(name))) %>%
#   filter(stringr::str_detect(name,"_model")) %>%
#   separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE) %>%
#   filter(Metric %in% "standard") %>%
#   rowwise() %>% 
#   # select(-model) %>%
#   # mutate(check = stringr::str_detect(Simplicity,"enhanced"))
#   mutate(
#     pred_test = ifelse(stringr::str_detect(Simplicity,"enhanced")
#                        , list(predict(model, standard_rolling_test_tbl, type = 'prob') %>% pull(2) %>% round())
#                        , list(predict(model, standard_rolling_test_tbl, type = 'response') %>% as.vector() %>% round() )
#     )
#     
#     , pred_validation = ifelse(stringr::str_detect(Simplicity,"enhanced"), list(predict(model, standard_rolling_validation_tbl, type = 'prob') %>% pull(2) %>% round() ), list(predict(model, standard_rolling_validation_tbl, type = 'response') %>% as.vector() %>% round() ))
#     , response_test = list(standard_rolling_test_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
#     , response_validation = list(standard_rolling_validation_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
#     , test_confusion_matrix = list(caret::confusionMatrix(reference = response_test %>% factor(), data=pred_test %>% factor()))
#     , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation %>% factor(), data=pred_validation %>% factor()))
#   )  %>%
#   ungroup() %>%
#   select(-model) %>%
#   mutate(Metric = "standard-rolling")

# model_tbl_final <- bind_rows(model_tbl,model_standard_season_tbl,model_standard_rolling_tbl)

# readr::write_rds(model_tbl, "Data/clean/model_prediction_tbl.rds")

# Final Predictions Table ----
model_tbl_final <- model_tbl %>%
  ungroup() %>%
  select(-test_confusion_matrix
         ,-validation_confusion_matrix
         ) %>%
  pivot_longer(c(-name,-Simplicity,-Metric,-`Model Type`,-Temp)
               , names_to = "temp",values_to = "value") %>%
  separate(temp,into = c("pred","type")) %>%
  pivot_wider(names_from = pred
              , values_from = value) %>% 
  rowwise() %>%
  mutate(confusion_matrix_ = list(caret::confusionMatrix(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                   , data=pred %>% unlist()%>% factor() 
                                                   ) 
                                  )
         , specificity_ = caret::specificity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             )
         , sensitivity_ = caret::sensitivity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             )
         , accuracy_ = confusion_matrix_$overall[[1]] %>% round(3) %>% .[1]
         , precision = caret::precision(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                        , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
         )
         ) %>%
  select(-Temp,-name)
  
# Create Ensemble Prediction Tibble ----
create_tibble <- function(x){
  
  return(tibble(name = x))
  
}
element_wise_add <- function(vec1, vec2) {
  map2_dbl(vec1, vec2, `+`)
}

model_ensemble_tbl <- lapply(ls(), create_tibble) %>% 
  bind_rows() %>% 
  rowwise() %>% 
  mutate(model = list(get(name))) %>%
  filter(stringr::str_detect(name,"_model")) %>%
  mutate(
    pred_test = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, test_tbl, type = 'prob') %>% pull(2)), list(predict(model, test_tbl, type = 'response') %>% as.vector() ))
    , pred_validation = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, validation_tbl, type = 'prob') %>% pull(2) ), list(predict(model, validation_tbl, type = 'response') %>% as.vector() ))
    , response_test = list(test_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
    , response_validation = list(validation_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
  ) %>%
  select(-model)  %>%
  separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE) %>%
  filter(!Metric %in% "standard") %>%
  summarize(pred_test = list(reduce(pred_test, element_wise_add)) %>% lapply(function(x) round(x/10))
            , pred_validation = list(reduce(pred_validation, element_wise_add)) %>% lapply(function(x) round(x/10))
            , response_test = list(reduce(response_test, element_wise_add)) %>% lapply(function(x) round(x/10))
            , response_validation = list(reduce(response_validation, element_wise_add)) %>% lapply(function(x) round(x/10))) %>%
  mutate(test_confusion_matrix = list(caret::confusionMatrix(reference = response_test  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))), data=pred_test  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                             )
                                      )
         , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                                     , data=pred_validation  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                                     )
                                              )
         )

model_ensemble_tbl_season <- lapply(ls(), create_tibble) %>% 
  bind_rows() %>% 
  rowwise() %>% 
  mutate(model = list(get(name))) %>%
  filter(stringr::str_detect(name,"_model")) %>%
  mutate(
    pred_test = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, test_tbl, type = 'prob') %>% pull(2)), list(predict(model, test_tbl, type = 'response') %>% as.vector() ))
    , pred_validation = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, validation_tbl, type = 'prob') %>% pull(2) ), list(predict(model, validation_tbl, type = 'response') %>% as.vector() ))
    , response_test = list(test_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
    , response_validation = list(validation_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
  ) %>%
  select(-model)  %>%
  separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE) %>%
  filter(Metric %in% "season") %>%
  filter(!Metric %in% "standard") %>%
  ungroup() %>%
  summarize(pred_test = list(reduce(pred_test, element_wise_add)) %>% lapply(function(x) round(x/5))
            , pred_validation = list(reduce(pred_validation, element_wise_add)) %>% lapply(function(x) round(x/5))
            , response_test = list(reduce(response_test, element_wise_add)) %>% lapply(function(x) round(x/5))
            , response_validation = list(reduce(response_validation, element_wise_add)) %>% lapply(function(x) round(x/5))) %>%
  mutate(test_confusion_matrix = list(caret::confusionMatrix(reference = response_test  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))), data=pred_test  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
  )
  )
  , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                              , data=pred_validation  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
  )
  )
  )

model_ensemble_tbl_rolling <- lapply(ls(), create_tibble) %>% 
  bind_rows() %>% 
  rowwise() %>% 
  mutate(model = list(get(name))) %>%
  filter(stringr::str_detect(name,"_model")) %>%
  mutate(
    pred_test = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, test_tbl, type = 'prob') %>% pull(2)), list(predict(model, test_tbl, type = 'response') %>% as.vector() ))
    , pred_validation = ifelse(stringr::str_detect(name,"enhanced"), list(predict(model, validation_tbl, type = 'prob') %>% pull(2) ), list(predict(model, validation_tbl, type = 'response') %>% as.vector() ))
    , response_test = list(test_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
    , response_validation = list(validation_tbl %>% pull(home_winner_response) %>% as.numeric() %>% -1)
  ) %>%
  select(-model)  %>%
  separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE) %>%
  filter(Metric %in% "rolling") %>%
  filter(!Metric %in% "standard") %>%
  ungroup() %>%
  summarize(pred_test = list(reduce(pred_test, element_wise_add)) %>% lapply(function(x) round(x/5))
            , pred_validation = list(reduce(pred_validation, element_wise_add)) %>% lapply(function(x) round(x/5))
            , response_test = list(reduce(response_test, element_wise_add)) %>% lapply(function(x) round(x/5))
            , response_validation = list(reduce(response_validation, element_wise_add)) %>% lapply(function(x) round(x/5))) %>%
  mutate(test_confusion_matrix = list(caret::confusionMatrix(reference = response_test  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))), data=pred_test  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
  )
  )
  , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                              , data=pred_validation  %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
  )
  )
  )

model_ensemble_tbl <- model_ensemble_tbl %>%
  select(-test_confusion_matrix, -validation_confusion_matrix) %>%
  pivot_longer(everything()) %>%
  separate(name, into=c("temp","type")) %>%
  pivot_wider(names_from = temp, values_from = value) %>%
  mutate(Simplicity = "combination"
         , Metric = "combination"
         , `Model Type` = "combination"
         ) %>%
  rowwise() %>%
  mutate(confusion_matrix_ = list(caret::confusionMatrix(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                         , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                         ) 
                                  )
         , specificity_ = caret::specificity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
                                             )
         , sensitivity_ = caret::sensitivity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
         )
         , accuracy_ = confusion_matrix_$overall[[1]] %>% round(3) %>% .[1]
         , precision = caret::precision(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                             , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
         )
  )
model_ensemble_tbl_season <- model_ensemble_tbl_season %>%
  select(-test_confusion_matrix, -validation_confusion_matrix) %>%
  pivot_longer(everything()) %>%
  separate(name, into=c("temp","type")) %>%
  pivot_wider(names_from = temp, values_from = value) %>%
  mutate(Simplicity = "combination"
         , Metric = "season"
         , `Model Type` = "combination"
  ) %>%
  rowwise() %>%
  mutate(confusion_matrix_ = list(caret::confusionMatrix(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                         , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
  ) 
  )
  , specificity_ = caret::specificity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                      , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
  )
  , sensitivity_ = caret::sensitivity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                      , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
  )
  , accuracy_ = confusion_matrix_$overall[[1]] %>% round(3) %>% .[1]
  , precision = caret::precision(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                 , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
  )
  )
model_ensemble_tbl_rolling <- model_ensemble_tbl_rolling %>%
  select(-test_confusion_matrix, -validation_confusion_matrix) %>%
  pivot_longer(everything()) %>%
  separate(name, into=c("temp","type")) %>%
  pivot_wider(names_from = temp, values_from = value) %>%
  mutate(Simplicity = "combination"
         , Metric = "rolling"
         , `Model Type` = "combination"
  ) %>%
  rowwise() %>%
  mutate(confusion_matrix_ = list(caret::confusionMatrix(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                         , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
  ) 
  )
  , specificity_ = caret::specificity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                      , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
  )
  , sensitivity_ = caret::sensitivity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                      , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
  )
  , accuracy_ = confusion_matrix_$overall[[1]] %>% round(3) %>% .[1]
  , precision = caret::precision(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                 , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
  )
  )
espn_prob_tmp1 <- test_tbl %>% 
  select(home_espn_probability, home_winner_response) %>%
  mutate(home_espn_probability = round(home_espn_probability/100)
         , home_winner_response = home_winner_response %>% as.numeric() -1 
  )
espn_prob_tmp2 <- validation_tbl %>% 
  select(home_espn_probability, home_winner_response) %>%
  mutate(home_espn_probability = round(home_espn_probability/100)
         , home_winner_response = home_winner_response %>% as.numeric() -1 
  )


model_tbl_final <- bind_rows(model_ensemble_tbl
                             
          , model_tbl_final
          , model_ensemble_tbl_rolling
          ,model_ensemble_tbl_season
          , tibble(type = c("test","validation")
                   , pred = c(list(espn_prob_tmp1$home_espn_probability),list(espn_prob_tmp2$home_espn_probability)
                   )
                   , response = c(list(espn_prob_tmp1$home_winner_response),list(espn_prob_tmp2$home_winner_response)
                   )
                   , Simplicity = "espn"
                   , Metric = "espn"
                   , `Model Type` = "espn"
          )%>%
            rowwise() %>%
            mutate(confusion_matrix_ = list(caret::confusionMatrix(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                                   , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
                                                                   ) 
                                            )
                   , specificity_ = caret::specificity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                       , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
                                                       )
                   , sensitivity_ = caret::sensitivity(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                       , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                       )
                   , accuracy_ = confusion_matrix_$overall[[1]] %>% round(3) %>% .[1]
                   , precision = caret::precision(reference = response %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.)))
                                                  , data=pred %>% unlist() %>% as_factor() %>% factor(levels = rev(levels(.))) 
                   )
            )
          ) 
