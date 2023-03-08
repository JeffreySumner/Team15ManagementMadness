library(ncaahoopR)
ncaa_team_ids <- ncaahoopR::ids
ncaa_dict <- ncaahoopR::dict
ncaa_colors <- ncaahoopR::ncaa_colors
ncaahoopR::court

years <- c("2015-16", "2016-17", "2017-18", "2018-19")

ncaahoopR::get_game_ids()

get_all_game_ids <- function(id){
  vec <- c()
  for(i in c("2015-16", "2016-17", "2017-18", "2018-19")){
    vec <- c(get_game_ids(team = id, season = i), vec)
  }
  return(vec)
}

get_all_game_ids("Air Force")


