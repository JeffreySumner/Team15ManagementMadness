# Required Packages 
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('glmnet')) install.packages('glmnet')
if (!require('vip')) install.packages('vip')

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
    , test_confusion_matrix = list(caret::confusionMatrix(reference = response_test %>% factor(), data=pred_test %>% factor()))
    , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation %>% factor(), data=pred_validation %>% factor()))
  )  %>%
  ungroup() %>%
  select(-model)  %>%
  separate(name, into = c("Simplicity","Metric","Model Type","Temp"), remove = FALSE)

# readr::write_rds(model_tbl, "Data/clean/model_prediction_tbl.rds")

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
            , response_validation = list(reduce(response_validation, element_wise_add)) %>% lapply(function(x) round(x/10))

  ) %>%
  mutate(test_confusion_matrix = list(caret::confusionMatrix(reference = response_test  %>% unlist() %>% factor(), data=pred_test  %>% unlist() %>% factor()))
         , validation_confusion_matrix = list(caret::confusionMatrix(reference = response_validation  %>% unlist() %>% factor(), data=pred_validation  %>% unlist() %>% factor())))
# readr::write_rds(model_tbl, "Data/clean/model_prediction_tbl.rds")
