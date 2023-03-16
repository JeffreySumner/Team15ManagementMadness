if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')

# run if needed - PBP is very large
box_score_tbl <- read_csv("Data/box_score_tbl.csv")
pbp_tbl <- read_csv("Data/pbp_tbl.csv")
roster_tbl <- read_csv("Data/roster_tbl.csv")
schedule_tbl <- read_csv("Data/schedule_tbl.csv")


split_file_path <- function(string){
  patterns <- c("../ncaahoopR_data/" = "", "/pbp_logs" = "",".csv" = "", "/rosters" = "", "/schedules" = "", "_schedule" = "", "_" = " ")
  temp_string <- stringr::str_replace_all(string, patterns)
}
  
pbp_tbl <- pbp_tbl %>%
  mutate(temp = split_file_path(file)) %>% 
  separate_wider_delim(cols = temp
                       , names = c("season","date","game_id")
                       , delim = "/"
                       )
pbp_tbl <- separate_wider_delim(data = data.frame(string = temp_string), cols = string,names = c("season","date","game_id"), delim = "/")

schedule_tbl_temp <- separate_wider_delim(data = schedule_tbl_temp , cols = temp,names = c("season","team_name"), delim = "/")
schedule_tbl <- schedule_tbl %>%
  mutate(temp = split_file_path(file)) %>% 
  separate_wider_delim(cols = temp
                       , names = c("season","team_name")
                       , delim = "/"
  )
