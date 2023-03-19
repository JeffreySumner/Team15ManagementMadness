# Required Packages ----
if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('hoopR')) install.packages('hoopR')
if (!require('tictoc')) install.packages('tictoc')
if (!require('hoopR')) devtools::install_github('sportsdataverse/hoopR')

pbp_win_probabilities_tbl <- read_csv("Data/pbp_win_probabilities_tbl.csv")

mbb_box_tbl <- load_mbb_team_box(
  seasons = c(2012:2023)
)


tictoc::tic()
test <- espn_mbb_betting(401522119) %>% mutate(game_id = 401522119)
tictoc::toc()
hoopR::login(user_email = Sys.getenv("KP_USER"), user_pw = Sys.getenv("KP_PW"))

kp_user_email()

kp_password()

has_kp_user_and_pw()

min_wp <- kp_game_attrs(year = 2021, attr = "MinWp")

library(parallel)
cl <- makeCluster(4)


game_ids_vec <- pbp_win_probabilities_tbl %>%
  pull(game_id)

myInputs <- game_ids_vec[1:100]

espn_mbb_betting_new <- function(game_id){
  
  temp <- hoopR::espn_mbb_betting(game_id) 
  
  temp["game_id"] <- game_id
  
  return(temp)
  
}

tictoc::tic()
results <- parLapply(cl, myInputs, espn_mbb_betting_new) %>% list()
tictoc::toc()

tictoc::tic()
results <- lapply(myInputs, espn_mbb_betting_new)
tictoc::toc()
stopCluster(cl)
print(results)