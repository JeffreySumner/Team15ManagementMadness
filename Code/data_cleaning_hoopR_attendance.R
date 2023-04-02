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

# Step 1: How to Pull the scoreboard/attendance data ----
## Update the mbb function ----
espn_mbb_scoreboard_new <- function(date){
  date <- as.character(date) %>%
    str_replace_all(pattern = "-","")
  tryCatch({
    temp <- espn_mbb_scoreboard(date)
    return(temp)
    
  }, error = function(e) {
    message(paste("Error getting play-by-play data for game", game_id))
    temp <- data.frame()
    return(temp)
  })
}
## Begin to pull mbb scoreboard/attendance data ----
date_vec <- mbb_box_score_2012_2022_tbl %>%
  pull(game_date) %>%
  unique()

# parlapply for parallel processing to get betting data
# bind data to dataframe
tictoc::tic()
espn_mbb_attendance_2012_2022_tbl <- parLapply(cl, date_vec, espn_mbb_scoreboard_new) %>% bind_rows()
tictoc::toc()

stopCluster(cl)

# Write attendance data ----
readr::write_csv(espn_mbb_attendance_2012_2022_tbl, "Data/espn_mbb_attendance_2012_2022_tbl.csv")
