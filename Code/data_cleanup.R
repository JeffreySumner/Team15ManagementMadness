if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')

# Read data ----
mbb_box_score_2012_2022_tbl <- read_csv("Data/mbb_box_score_2012_2022_tbl.csv")
mbb_attendance_2012_2022_tbl <- read_csv("Data/mbb_attendance_2012_2022_tbl.csv")
ap_poll_2012_2022_raw_tbl <- read_csv("Data/ap_poll_2012_2022_raw_tbl.csv")
colleges_tbl <- read_delim("Data/us-colleges-and-universities.csv",";")

# Clean Boxscore data ----

box_score_clean <- mbb_box_score_2012_2022_tbl %>%
  separate(
    field_goals_made_field_goals_attempted
    , sep = "-"
    , into = c("FGM", "FGA")
  ) %>%
  separate(
    three_point_field_goals_made_three_point_field_goals_attempted
    , sep = "-"
    , into = c("FGM3", "FGA3")
  ) %>%
  separate(
    free_throws_made_free_throws_attempted
    , sep = "-"
    , into = c("FTM", "FTA")
  ) %>%
  select(
    game_id
    , game_date
    , team_id
    , opponent_id
    , home_away
    , FGM
    , FGA
    , FGM3
    , FGA3
    , FTM
    , FTA
    , offensive_rebounds
    , defensive_rebounds
    , total_rebounds
    , team_rebounds
    , assists
    , steals
    , blocks
    , turnovers
    , team_turnovers
    , total_turnovers
    , fouls
    , technical_fouls
    , flagrant_fouls
    , largest_lead
  ) %>%
  mutate(
    across(FGM:FTA, as.numeric)
    , home_away = ifelse(home_away == "HOME", "home", "away")
  ) %>%
  pivot_longer(c(-home_away,-game_id,-game_date)) %>%
  filter(!(home_away == "home" & name == "opponent_id"), !(home_away == "away" & name == "team_id")) %>%
  unite(
    "temp"
    , home_away:name
  ) %>%
  pivot_wider(
    names_from = temp
    , values_from = value
  ) %>%
  # estimate the number of possessions
  mutate(
    home_poss = (home_FGA - home_offensive_rebounds) + home_total_turnovers + (.44 * home_FTA) # kenpom suggested factor .44
    , away_poss = (away_FGA - away_offensive_rebounds) + away_total_turnovers + (.44 * away_FTA)
    , home_points = (home_FGM - home_FGM3) * 2 + (home_FGM3) * 3 + home_FTM * 1
    , away_points = (away_FGM - away_FGM3) * 2 + (away_FGM3) * 3 + away_FTM * 1
    , home_defensive_rating = 100 * (away_points/away_poss)
    , home_offensive_rating = 100 * (home_points/home_poss)
    , away_defensive_rating = home_offensive_rating
    , away_offensive_rating = home_defensive_rating
  )


# Clean attendance data and projections ----

attendance_clean <- mbb_attendance_2012_2022_tbl %>%
  select(
    game_id
    , city
    , state
    , full_name = fullName
    , neutral_site
    , type
    , home_projection = homeTeam_gameProjection
    , away_projection = awayTeam_gameProjection
    , attendance
    , capacity
    
  )

# Clean AP Poll Data ----

ap_poll_clean <- ap_poll_2012_2022_raw_tbl %>%
  filter(!is.na(school)) %>%
  pivot_longer(c(-school, -conference, -year)) %>%
  filter(
    !school %in% "School"
    , !is.na(value)
  ) %>%
  rename(
    team_name = school
    , season = year
    , week = name
    , ap_rank = value
  )



# Clean Colleges and Universities

locations <- mbb_attendance_2012_2022_tbl %>%
  select(fullName, city, state) %>%
  distinct() %>%
  filter(!is.na(fullName)) 

geocoded_data <- locations %>%
  # top_n(5) %>%
  mutate(
    location = glue::glue("{fullName}, {city}, {state}")
    , geocode_data = geocode(location = location, output = "more")
  ) %>%
  unnest_wider(geocode_data) 

# readr::write_csv(geocoded_data, "Data/geocoded_locations_tbl.csv")
geocoded_tbl <- readr::read_csv("Data/geocoded_locations_tbl.csv")
