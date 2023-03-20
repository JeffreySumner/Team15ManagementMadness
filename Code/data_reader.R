if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')

# run if needed - PBP is very large
box_score_tbl <- read_csv("Data/box_score_tbl.csv")
pbp_tbl <- read_csv("Data/pbp_tbl.csv")

pbp_win_probabilities <- pbp_tbl %>%
  filter(play_id == 1) %>%
  select(
    home
    , away
    , date
    , game_id
    , win_prob
    , naive_win_prob
  )

pbp_tbl %>%
  filter(!is.na(attendance)) %>%
  select(date) %>%
  distinct()
