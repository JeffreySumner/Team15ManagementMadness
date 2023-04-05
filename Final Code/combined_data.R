if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')

ap_poll_tbl <- readr::read_csv("Data/clean/ap_poll_clean_tbl.csv")
away_team_stats_tbl <- readr::read_csv("Data/clean/away_team_stats_tbl.csv")
home_team_stats_tbl <- readr::read_csv("Data/clean/home_team_stats_tbl.csv")
game_information_tbl <- readr::read_csv("Data/clean/game_information_tbl.csv")
date_tbl <- readr::read_csv("Data/clean/date_tbl.csv")

# Model Data Initial----
model_data_tbl <- game_information_tbl %>%
  left_join(
    home_team_stats_tbl %>% select(game_id,game_date,season, starts_with("home_"))
    , by = "game_id"
  ) %>%
  left_join(
    away_team_stats_tbl %>% select(game_id, starts_with("away_"))
    , by = "game_id"
  )  %>%
  left_join(
    date_tbl %>% select(-season)
    , by = "game_date"
  ) %>%
  left_join(
    ap_poll_tbl %>%
      select(-conference,-team_name) %>%
      rename(home_ap_rank = ap_rank)
    , by = c("season", "week", "homeTeam_id" = "team_id")
  ) %>%
  left_join(
    ap_poll_tbl %>%
      select(-conference,-team_name) %>%
      rename(away_ap_rank = ap_rank)
    , by = c("season", "week", "awayTeam_id" = "team_id")
  )

# Model Data Readr ----
# run this if you want to store the data then read on ONLY this section... doesn't matter, up to you
# this file is too large for github
# readr::write_csv(model_data_tbl, "Data/clean/model_data_tbl.csv")
# model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")
# Split Model Data ----

set.seed(15) # for reproducibility
initial_split_data <- initial_split(model_data_tbl, prop = .7)
train_tbl <- training(initial_split_data)
test_tbl <- testing(initial_split_data)

