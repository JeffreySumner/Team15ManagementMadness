if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('glue')) install.packages('glue')
if (!require('forcats')) install.packages('forcats')
if (!require('DataExplorer')) install.packages('DataExplorer')

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
  ) %>%
  select(!contains("largest_lead")) %>%
  filter(!is.na(home_defensive_rating_roll5), !is.na(away_defensive_rating_roll5), !is.na(away_distance), !is.na(home_distance)) %>%
  mutate(home_ap_rank_fct = ifelse(is.na(home_ap_rank),"Not Ranked",as.character(home_ap_rank)) %>% forcats::as_factor()
         , away_ap_rank_fct = ifelse(is.na(away_ap_rank),"Not Ranked",as.character(away_ap_rank)) %>% forcats::as_factor()
  ) %>% 
  mutate(home_winner = home_points_ > away_points_)

# validate that NAs are taken care of

# colSums(is.na(model_data_tbl %>%
#                 select(!contains("largest_lead")) %>%
#                 filter(!is.na(home_defensive_rating_roll5), !is.na(away_defensive_rating_roll5), !is.na(away_distance), !is.na(home_distance)) %>%
#                 mutate(home_ap_rank_fct = ifelse(is.na(home_ap_rank),"Not Ranked",as.character(home_ap_rank)) %>% forcats::as_factor()
#                        , away_ap_rank_fct = ifelse(is.na(away_ap_rank),"Not Ranked",as.character(away_ap_rank)) %>% forcats::as_factor()
#                 ) %>% 
#                 mutate(home_winner = home_points_ > away_points_)
# )
# )

# Model Data Readr ----
# run this if you want to store the data then read on ONLY this section... doesn't matter, up to you
# this file is too large for github
# make sure to get the zip file and unzip then place it in data/clean

readr::write_csv(model_data_tbl, "Data/clean/model_data_tbl.csv")
model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")
# Split Model Data ----

model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")
set.seed(15) # for reproducibility
initial_split_data <- initial_split(model_data_tbl, prop = .6)
train_tbl <- training(initial_split_data)
test_temp_tbl <- testing(initial_split_data)

set.seed(15)
validation_split <- initial_split(test_temp_tbl, prop = .5)
test_tbl <- training(validation_split)
validation_tbl <- testing(validation_split)

rm(initial_split_data, validation_split, test_temp_tbl)
gc()

DataExplorer::create_report(
  model_data_tbl
  , output_file = "model_data_tbl_eda"
  , output_dir = "Other Resources/"
  , y = "home_winner_response"
  , report_title = "EDA Report - MBB Home Winner"
)

