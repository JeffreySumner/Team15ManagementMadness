if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')

# run if needed - PBP is very large
mbb_box_score_2012_2022_tbl <- read_csv("Data/mbb_box_score_2012_2022_tbl.csv")

espn_mbb_betting_2012_2015_tbl <- read_rds("Data/espn_mbb_betting_2012_2015_tbl.rds")

espn_mbb_win_probability_2012_2022_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  select(game_id,predictor) %>%
  unnest_wider(predictor) %>% 
  filter(!is.na(away_team_chance_loss))

