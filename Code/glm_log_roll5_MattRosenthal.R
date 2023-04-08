rm(list=ls()) 
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')

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

glm(home_winner_response ~ home_assists_roll5 +
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
    data = train_tbl, family = "binomial") %>%
  predict(newdata = validation_tbl, type = "response") %>%
  as_vector() %>%
  tibble(pred = .) %>%
  mutate(pred = ifelse(pred > 0.5, 1, 0),
         model_type = "glm log"
         , col = "rolling")