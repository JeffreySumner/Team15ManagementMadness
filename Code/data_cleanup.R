if (!require('devtools')) install.packages('devtools')
if (!require('ncaahoopR')) devtools::install_github('lbenz730/ncaahoopR')
if (!require('tidyverse')) install.packages('tidyverse')

# Read data ----
mbb_box_score_2012_2022_tbl <- read_csv("Data/raw/mbb_box_score_2012_2022_tbl.csv")
mbb_attendance_2012_2022_tbl <- read_csv("Data/raw/mbb_attendance_2012_2022_tbl.csv")
ap_poll_2012_2022_raw_tbl <- read_csv("Data/raw/ap_poll_2012_2022_raw_tbl.csv")
# colleges_tbl <- read_delim("Data/us-colleges-and-universities.csv",";")

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
    , season
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
  pivot_longer(c(-home_away,-game_id,-game_date,-season)) %>%
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


# Clean Team Stats ----

box_score_clean1 <- mbb_box_score_2012_2022_tbl %>%
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
    , season
    , season_type
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
    , home_away = ifelse(home_away == "HOME", "away", "home")
  ) %>%
  pivot_longer(c(-home_away,-game_id,-game_date,-season, -season_type,-team_id,-opponent_id)) 

box_score_clean2 <- box_score_clean1 %>%
  mutate(home_away = ifelse(home_away == "home","away","home")) %>%
  rename(opp = value)

team_stats_tbl <- box_score_clean1 %>%
  left_join(
    box_score_clean2 %>% select(-team_id, -home_away, -season_type) 
    , by = c("game_id","game_date","season","name","team_id" = "opponent_id")
  ) %>%
  pivot_longer(c(value,opp), names_to = "type") %>%
  mutate(type = ifelse(type == "value","","_opp")) %>%
  unite(
    "temp"
    , c(name,type)
    , sep = ""
  ) %>%
  pivot_wider(
    names_from = temp
    , values_from = value
  ) %>%
  # estimate the number of possessions
  mutate(
    poss = (FGA - offensive_rebounds) + total_turnovers + (.44 * FTA) # kenpom suggested factor .44
    , poss_opp = (FGA_opp - offensive_rebounds_opp) + total_turnovers_opp + (.44 * FTA_opp)
    , points = (FGM - FGM3) * 2 + (FGM3) * 3 + FTM * 1
    , points_opp = (FGM_opp - FGM3_opp) * 2 + (FGM3_opp) * 3 + FTM_opp * 1
    , defensive_rating = 100 * (points_opp/poss_opp)
    , offensive_rating = 100 * (points/poss)
    , defensive_rating_opp = offensive_rating
    , offensive_rating_opp = defensive_rating
  ) %>%
  pivot_longer(c(-game_id, -game_date, -season, -season_type, -team_id, -opponent_id, -home_away)) %>%
  group_by(season,team_id,name, game_date) %>%
  arrange(game_id, .by_group = TRUE) %>%
  ungroup() %>%
  group_by(season, team_id, name) %>%
  mutate(
    game_number = row_number()
    , first_game = ifelse(game_number == 1, TRUE, FALSE)
    , lag1_val = lag(value, 1)
  ) %>%
  group_by(season, team_id, name, first_game) %>%
  mutate(
    season_avg = cummean(lag1_val)
  ) %>%
  # ungroup() %>%
  # group_by(season, team_id, name) %>%
  mutate(
    roll5_val = zoo::rollmean(lag1_val, k = 5,fill = NA, align = "right")
    # , roll1_val = zoo::rollmean(lag1_val, k = 1,fill = NA, align = "right")
    # , roll2_val = zoo::rollmean(lag1_val, k = 2,fill = NA, align = "right")
    # , roll3_val = zoo::rollmean(lag1_val, k = 3,fill = NA, align = "right")
    # , roll4_val = zoo::rollmean(lag1_val, k = 4,fill = NA, align = "right")
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

ap_poll_clean_tbl <- ap_poll_2012_2022_raw_tbl %>%
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

readr::write_csv(ap_poll_clean_tbl, "Data/clean/ap_poll_clean_tbl.csv")

# Clean Colleges and Universities ----

# locations <- mbb_attendance_2012_2022_tbl %>%
#   select(fullName, city, state) %>%
#   distinct() %>%
#   filter(!is.na(fullName)) 

# geocoded_data <- locations %>%
#   # top_n(5) %>%
#   mutate(
#     location = glue::glue("{fullName}, {city}, {state}")
#     , geocode_data = geocode(location = location, output = "more")
#   ) %>%
#   unnest_wider(geocode_data) 

# readr::write_csv(geocoded_data, "Data/geocoded_locations_tbl.csv")
geocoded_tbl <- readr::read_csv("Data/raw/geocoded_locations_tbl.csv")

# Game Distances ----
team_home_locations <- mbb_attendance_2012_2022_tbl %>%
  select(fullName, city, state, homeTeam_id, awayTeam_id) %>%
  group_by(homeTeam_id,fullName) %>%
  summarize(counts=  n()) %>% 
  filter(counts %in% max(counts)) %>%
  rename(team_id = homeTeam_id, arena_name = fullName) %>%
  select(-counts) %>%
  ungroup() %>%
  left_join(geocoded_tbl %>% select(fullName, lat, lon, city, state), by = c("arena_name" = "fullName")) %>%
  filter(!is.na(arena_name))

game_distance_tbl <- mbb_attendance_2012_2022_tbl %>%
  select(fullName, city, state, homeTeam_id, awayTeam_id) %>%
  left_join(team_home_locations %>%
              rename(homeTeam_id = team_id
                     , home_arena_name = arena_name
                     , home_lat = lat
                     , home_lon = lon
                     , home_city = city
                     , home_state = state
                     )
            , by = "homeTeam_id")%>%
  left_join(team_home_locations %>% 
              rename(awayTeam_id = team_id
                     , away_arena_name = arena_name
                     , away_lat = lat
                     , away_lon = lon
                     , away_city = city
                     , away_state = state
                     )
            , by = "awayTeam_id") %>%
  left_join(geocoded_tbl %>% 
              select(fullName, arena_lat = lat, arena_lon = lon, arena_city = city, arena_state = state)
            , by = "fullName"
            ) %>%
  filter(!is.na(fullName)) %>% 
  # head(5)  %>% 
  # mutate(point_1 = c(arena_lon,arena_lat))
  mutate(home_distance = geosphere::distVincentyEllipsoid(cbind(arena_lon,arena_lat),cbind(home_lon,home_lat))  / 1609
         , away_distance = geosphere::distVincentyEllipsoid(cbind(arena_lon,arena_lat),cbind(away_lon,away_lat)) / 1609
         )

readr::write_csv(game_distance_tbl, "Data/clean/game_distance_tbl.csv")
# Team Statistics ----

team_stats_complete_tbl <- team_stats_tbl  %>%
  pivot_longer(
    cols = c(value
             , lag1_val
             , season_avg
             # , roll1_val
             # , roll2_val
             , roll3_val
             # , roll4_val
             , roll5_val)
    , names_to = "value_type"
    , values_to = "value"
  ) %>% 
  mutate(value_type = case_when(
    value_type == "value" ~ ""
    , value_type == "lag1_val" ~ "_lag1"
    , value_type == "season_avg" ~ "_season_avg"
    # , value_type == "roll1_val" ~ "_roll1"
    # , value_type == "roll2_val" ~ "_roll2"
    , value_type == "roll3_val" ~ "_roll3"
    # , value_type == "roll4_val" ~ "_roll4"
    , value_type == "roll5_val" ~ "_roll5"
    , TRUE ~ NA_character_)
  ) %>%
  unite(
    "temp"
    , c(name,value_type)
    , sep = ""
  ) %>%
  pivot_wider(
    names_from = temp
    , values_from = value
  )

# Contingency Table ----
contingency_tbl <- team_stats_complete_tbl %>%
  filter(game_number>5) %>%
  ungroup() %>%
  mutate(winner = ifelse(points - points_opp > 0, TRUE, FALSE)) %>% 
  select(game_id,team_id, points, points_opp, winner, home_away) %>%
  left_join(attendance_clean, by = "game_id") %>%
  mutate(win_prob = ifelse(home_away == "home",home_projection,away_projection)
         , espn_pred_winner = ifelse(win_prob > 50, TRUE, FALSE)
  ) %>%
  select(game_id, winner, espn_pred_winner) 

table(contingency_tbl$winner, contingency_tbl$espn_pred_winner)

contingency_condensed_tbl <- contingency_tbl %>%
  filter(!is.na(espn_pred_winner)) %>%
  mutate(flag = winner == espn_pred_winner
         , flag2 = case_when(
           winner == FALSE & espn_pred_winner == FALSE ~ "ESPN Predicted Loss and is Loss"
           , winner == TRUE & espn_pred_winner == FALSE ~ "ESPN Predicted Loss and is Win"
           , winner == FALSE & espn_pred_winner == TRUE ~ "ESPN Predicted Win and is Loss"
           , winner == TRUE & espn_pred_winner == TRUE ~ "ESPN Predicted Win and is Win"
         )
         
  ) %>% 
  group_by(flag2) %>% 
  summarize(counts = n())

espn_precision <- contingency_condensed_tbl %>% filter(flag2 %in% "ESPN Predicted Win and is Win") %>% pull(counts)/
  ((contingency_condensed_tbl %>% filter(flag2 %in% "ESPN Predicted Win and is Win") %>% pull(counts)) + (contingency_condensed_tbl %>% filter(flag2 %in% "ESPN Predicted Win and is Loss") %>% pull(counts)))

glue::glue("ESPN's precision is {round(espn_precision,4)}")

espn_specificity <- contingency_condensed_tbl %>% filter(flag2 %in% "ESPN Predicted Loss and is Loss") %>% pull(counts) /
  (contingency_condensed_tbl %>% filter(flag2 %in% c("ESPN Predicted Loss and is Loss","ESPN Predicted Win and is Loss")) %>% pull(counts) %>% sum())
glue::glue("ESPN's specificity is {round(espn_specificity,4)}")
espn_accuracy <- (contingency_condensed_tbl %>% filter(flag2 %in% c("ESPN Predicted Win and is Win", "ESPN Predicted Loss and is Loss")) %>% pull(counts) %>% sum())/
  (contingency_condensed_tbl %>% pull(counts) %>% sum())
glue::glue("ESPN's accuracy is {round(espn_accuracy,4)}")


# Correlation ----

team_stats_complete_tbl %>%
  ungroup() %>%
  select(contains("roll5"), -contains("FTM"), -contains("FGA"), -contains("FGM"), -contains("FTA"), -contains("team")) %>%
  filter(!is.na(turnovers_roll5)) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(type = "lower", lab = FALSE) +
  labs(title = "Correlation Plot of Win Probability Predictors")
# ggthemes::theme_fivethirtyeight()
ggsave(filename = "Visualizations/Data Exploration - Correlation Plot of Win Probability Predictors.png", width = 10, height = 5, units = "in",bg = 'white')

team_stats_complete_tbl %>%
  ungroup() %>%
  select(contains("roll5"), -contains("FTM"), -contains("FGA"), -contains("FGM"), -contains("FTA"), -contains("team")) %>%
  filter(!is.na(turnovers_roll5)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(.~name, scales = 'free') +
  labs(title = "Density Plot of Win Probability Predictors") +  
  ggthemes::theme_fivethirtyeight() 
ggsave(filename = "Visualizations/Data Exploration - Density Plot of Win Probability Predictors.png", width = 10, height = 5, units = "in",bg = 'white')

# Model attempt ----

model_tbl <- team_stats_complete_tbl %>%
  ungroup() %>%
  filter(game_id %in% (contingency_tbl %>%
                         filter(!is.na(espn_pred_winner)) %>% 
                         pull(game_id))
  ) %>%
  mutate(
    winner = ifelse(points - points_opp > 0, 1, 0)
  ) %>%
  select(winner,home_away, contains("roll5"), -contains("FTM"), -contains("FGA"), -contains("FGM"), -contains("FTA"), -contains("team")) %>%
  filter(!is.na(turnovers_roll5)) 

logit_model <- glm(winner ~ ., family = binomial(link = "logit"), data = model_tbl) 
probit_model <- glm(winner ~ ., family = binomial(link = "probit"), data = model_tbl) 

predict(logit_model,model_tbl, type = "response") %>% as.vector()

logit_contingency_tbl <- model_tbl %>%
  bind_cols(logit_pred = predict(logit_model,model_tbl, type = "response") %>% as.vector()) %>%
  mutate(
    win_prob = ifelse(logit_pred>.5,1,0)
  ) %>%
  mutate(flag = winner == win_prob
         , flag2 = case_when(
           winner == FALSE & win_prob == FALSE ~ "Logit Predicted Loss and is Loss"
           , winner == TRUE & win_prob == FALSE ~ "Logit Predicted Loss and is Win"
           , winner == FALSE & win_prob == TRUE ~ "Logit Predicted Win and is Loss"
           , winner == TRUE & win_prob == TRUE ~ "Logit Predicted Win and is Win"
         )
  ) %>%
  ungroup() %>%
  group_by(flag2) %>%
  summarize(counts = n())



logit_precision <- logit_contingency_tbl%>% filter(flag2 %in% "Logit Predicted Win and is Win") %>% pull(counts)/
  ((logit_contingency_tbl %>% filter(flag2 %in% "Logit Predicted Win and is Win") %>% pull(counts)) + (logit_contingency_tbl  %>% filter(flag2 %in% "Logit Predicted Win and is Loss") %>% pull(counts)))

glue::glue("Logit's precision is {round(logit_precision,4)}")

logit_specificity <- logit_contingency_tbl %>% filter(flag2 %in% "Logit Predicted Loss and is Loss") %>% pull(counts) /
  (logit_contingency_tbl  %>% filter(flag2 %in% c("Logit Predicted Loss and is Loss","Logit Predicted Win and is Loss")) %>% pull(counts) %>% sum())
glue::glue("Logit's specificity is {round(logit_specificity,4)}")

logit_accuracy <- (logit_contingency_tbl  %>% filter(flag2 %in% c("Logit Predicted Win and is Win", "Logit Predicted Loss and is Loss")) %>% pull(counts) %>% sum())/
  (logit_contingency_tbl  %>% pull(counts) %>% sum())
glue::glue("Logit's accuracy is {round(logit_accuracy,4)}")
