# Required Packages and setup ----
if (!require('devtools')) install.packages('devtools')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('hoopR')) install.packages('hoopR')
if (!require('tictoc')) install.packages('tictoc')
if (!require('hoopR')) devtools::install_github('sportsdataverse/hoopR')
if (!require('glue')) install.packages('glue')
if (!require('httr')) install.packages('httr')
if (!require('rvest')) install.packages('rvest')
# makeCluster initializes n # of cores
cl <- makeCluster(detectCores()-4)

# clusterEvalQ will add functions or packages to each cluster created
clusterEvalQ(
  cl
  , {
    library(tidyverse)
    library(hoopR)
    library(glue)
    library(httr)
    library(jsonlite)
  }
)

mbb_box_score_2012_2022_tbl <- read_csv("Data/raw/mbb_box_score_2012_2022_tbl.csv")

game_ids_vec <- mbb_box_score_2012_2022_tbl %>%
  pull(game_id) %>%
  unique()


get_attendance_espn_api <- function(game_id){

  tryCatch({
    url = glue::glue("https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={game_id}")
    
    txt = httr::GET(url) %>% httr::content(as = "text")
    
    game_info <- jsonlite::fromJSON(txt)
    temp <- game_info %>% 
      enframe() %>% 
      pivot_wider() %>% 
      select(gameInfo, predictor, odds) %>%  #, pickcenter , odds
      # unnest_wider(header) %>%
      unnest_wider(gameInfo) %>% 
      unnest_wider(venue) %>% 
      unnest_wider(predictor) %>%
      unnest_wider(homeTeam,names_sep = "_") %>% 
      unnest_wider(awayTeam,names_sep = "_") %>% 
      unnest_wider(address) %>% 
      unnest_wider(odds) %>%
      select(
        -officials
        ,-images
        ,-grass
        ,-id
        ,-header
        ) %>%
      # unnest_wider(officials, names_sep = "_") %>%
      mutate(game_id = game_id, type = 1)
    temp2 <- game_info %>% 
      enframe() %>% 
      pivot_wider() %>% 
      select(header) %>%
      unnest_wider(header)
    temp$neutral_site <- temp2$competitions$neutralSite
    return(temp)
  }, error = function(e) {
    message(paste("Error getting data for date", game_id))
    temp <- data.frame(game_id = game_id, type = 0) #date = date,
    return(temp)
  })
}

tictoc::tic()
mbb_attendance_2012_2022_tbl <- parLapply(cl, game_ids_vec, get_attendance_espn_api) %>% bind_rows()
tictoc::toc()

data.table::fwrite(mbb_attendance_2012_2022_tbl,"Data/raw/mbb_attendance_2012_2022_tbl.csv",append = TRUE)
