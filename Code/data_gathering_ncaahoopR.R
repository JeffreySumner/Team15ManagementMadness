# Required Packages ----
if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')

library(ncaahoopR)
library(tidyverse)

# Initial Package Data ----
ncaa_team_ids <- ncaahoopR::ids
ncaa_dict <- ncaahoopR::dict
ncaa_colors <- ncaahoopR::ncaa_colors

# ncaahoopR_data repo files ----
# note: this requires the respository to be pulled in order to run the code below
files <- list.files("../ncaahoopR_data",recursive = TRUE,full.names = TRUE)
years <- c( "2012-13", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23") #"2010-11", "2011-12",

box_score_files <- str_subset(files, "/box_scores/") %>% str_subset(pattern = str_c(years, collapse = "|"))
pbp_files <- str_subset(files, "/pbp_logs/") %>% str_subset(pattern = str_c(years, collapse = "|"))
roster_files <- str_subset(files, "/rosters/") %>% str_subset(pattern = str_c(years, collapse = "|"))
schedule_files <- str_subset(files, "/schedules/") %>% str_subset(pattern = str_c(years, collapse = "|"))


# get_data function ----
get_data <- function(file_path){
  
  data <- data.table::fread(file_path) %>%
    mutate(
      across(.fns = as.character)
      , file = file_path
    )
  
  return(data)
  
}

# pull ncaahoopR_data data + write to repo Data/ dir ----
box_score_tbl <- lapply(box_score_files, get_data) %>% bind_rows()
data.table::fwrite(box_score_tbl, "Data/box_score_tbl.csv")
pbp_tbl <- lapply(pbp_files, get_data) %>% bind_rows()
data.table::fwrite(pbp_tbl, "Data/pbp_tbl.csv")
roster_tbl <- lapply(roster_files, get_data) %>% bind_rows()
data.table::fwrite(roster_tbl, "Data/roster_tbl.csv")
schedule_tbl <- lapply(schedule_files, get_data) %>% bind_rows()
data.table::fwrite(schedule_tbl, "Data/schedule_tbl.csv")
