# Required Packages and setup ----
if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('hoopR')) install.packages('hoopR')
if (!require('tictoc')) install.packages('tictoc')
if (!require('hoopR')) devtools::install_github('sportsdataverse/hoopR')
require(parallel)

hoopR::login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))


# Get MBB box-score data for game_ids----

mbb_box_score_2012_2022_tbl <- readr::read_csv("Data/mbb_box_score_2012_2022_tbl.csv")

# Create Clusters for Parallel Processing ----

# makeCluster initializes n # of cores
cl <- makeCluster(detectCores()-4)

# clusterEvalQ will add functions or packages to each cluster created
clusterEvalQ(
  cl
  , {
    library(tidyverse)
    library(hoopR)
  }
)

# Step 1: How to Pull the betting data ----
## Update the mbb function to convert to dataframe and add game_id ----
espn_mbb_betting_new <- function(game_id){
  
  tryCatch({
    temp <- espn_mbb_betting(game_id)
    temp <- temp %>%
      enframe() %>%
      pivot_wider(names_from = name, values_from = value) %>%
      mutate(game_id = as.numeric(game_id)
             , type = 1)
    return(temp)
    
  }, error = function(e) {
    message(paste("Error getting play-by-play data for game", game_id))
    temp <- list(
      pickcenter = data.frame()
      , againstTheSpread = data.frame()
      , predictor = data.frame()
    ) %>%
      enframe() %>%
      pivot_wider(names_from = name, values_from = value) %>%
      mutate(game_id = as.numeric(game_id)
             , type = 0)
    return(temp)
  })
}
## Begin to pull mbb betting data ----
game_ids_vec <- mbb_box_score_2012_2022_tbl %>%
  pull(game_id) %>%
  unique()

# parlapply for parallel processing to get betting data
# bind data to dataframe
tictoc::tic()
espn_mbb_betting_2012_2022_tbl <- parLapply(cl, game_ids_vec, espn_mbb_betting_new) %>% bind_rows()
tictoc::toc()

stopCluster(cl)

## Save betting data ----

# Save all (file too large)
readr::write_rds(espn_mbb_betting_2012_2022_tbl,"Data/espn_mbb_betting_2012_2022_tbl.rds")

# Save errors separately
espn_mbb_betting_2012_2022_errors_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  filter(type %in% 0)
readr::write_rds(espn_mbb_betting_2012_2022_errors_tbl,"Data/espn_mbb_betting_2012_2022_errors_tbl.rds")

# Save non-errors

# overwrites original tbl
espn_mbb_betting_2012_2022_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  filter(type %in% 1)

# saves 2012-2015
espn_mbb_betting_2012_2015_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  filter(game_id %in% (mbb_box_score_2012_2022_tbl %>% filter(season %in% 2012:2015) %>% pull(game_id) %>% unique()))
readr::write_rds(espn_mbb_betting_2012_2015_tbl,"Data/espn_mbb_betting_2012_2015_tbl.rds")

# saves 2016-2019
espn_mbb_betting_2016_2019_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  filter(game_id %in% (mbb_box_score_2012_2022_tbl %>% filter(season %in% 2016:2019) %>% pull(game_id) %>% unique()))
readr::write_rds(espn_mbb_betting_2016_2019_tbl,"Data/espn_mbb_betting_2016_2019_tbl.rds")

# saves 2020-2022
espn_mbb_betting_2020_2022_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  filter(game_id %in% (mbb_box_score_2012_2022_tbl %>% filter(season %in% 2020:2022) %>% pull(game_id) %>% unique()))
readr::write_rds(espn_mbb_betting_2020_2022_tbl,"Data/espn_mbb_betting_2020_2022_tbl.rds")

# Step 2: How to clean up the betting data ----
## Read in files as needed ----

# run this if you have the full file
espn_mbb_betting_2012_2022_tbl <- readr::read_rds("Data/espn_mbb_betting_2012_2022_tbl.rds")

# run this if you have the files split apart (most likely this is what you have)
espn_mbb_betting_2012_2022_tbl <- bind_rows(
  
  readr::read_rds("Data/espn_mbb_betting_2012_2015_tbl.rds")
  , readr::read_rds("Data/espn_mbb_betting_2016_2019_tbl.rds")
  , readr::read_rds("Data/espn_mbb_betting_2020_2022_tbl.rds")
  , readr::read_rds("Data/espn_mbb_betting_2012_2022_errors_tbl.rds")
  
)

## Clean up win probability data ----
espn_mbb_win_probability_2012_2022_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  select(game_id,predictor) %>%
  unnest_wider(predictor)
readr::write_csv(espn_mbb_win_probability_2012_2022_tbl,"Data/espn_mbb_win_probability_2012_2022_tbl.csv")

## Clean up spread data ----
espn_mbb_spread_2012_2022_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  select(game_id,againstTheSpread) %>%
  unnest_wider(againstTheSpread)
readr::write_csv(espn_mbb_spread_2012_2022_tbl,"Data/espn_mbb_spread_2012_2022_tbl.csv")

## Clean up pickcenter data ----
espn_mbb_pickcenter_2012_2022_tbl <- espn_mbb_betting_2012_2022_tbl %>%
  select(game_id,pickcenter) %>%
  unnest_wider(pickcenter)
readr::write_csv(espn_mbb_pickcenter_2012_2022_tbl,"Data/espn_mbb_pickcenter_2012_2022_tbl.csv")


