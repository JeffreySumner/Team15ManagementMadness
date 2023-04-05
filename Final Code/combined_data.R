if (!require('tidyverse')) install.packages('tidyverse')

ap_poll_tbl <- readr::read_csv("Data/clean/ap_poll_clean_tbl.csv")
away_team_stats_tbl <- readr::read_csv("Data/clean/away_team_stats_tbl.csv")
home_team_stats_tbl <- readr::read_csv("Data/clean/home_team_stats_tbl.csv")
game_distance_tbl <- readr::read_csv("Data/clean/game_distance_tbl.csv")

