# map visual of our prediction accuracy | check for bias in our 
map_winner_tbl <- all_models_tbl %>%
  left_join(model_data_tbl %>% mutate(id = row_number()) %>% select(id, season,arena_lat, arena_lon ), by = 'id') %>%
  group_by(col_type,data_type, id, season) %>%
  summarize(pred = mean(pred) 
            , espn_prob = mean(espn_prob)
            , winner = mean(winner %>% as.numeric() -1) 
            , arena_lat = mean(arena_lat)
            , arena_lon = mean(arena_lon)
            ) %>%
  mutate(pred = ifelse(pred >= .5,1,0)
            , espn_prob = ifelse(espn_prob >= 50,1,0)
            , pred_espn_prob = round((pred+espn_prob)/2)
  ) %>%
  mutate(count = winner == pred ) %>% filter(count == TRUE)

map_loser_tbl <- all_models_tbl %>%
  left_join(model_data_tbl %>% mutate(id = row_number()) %>% select(id, season,arena_lat, arena_lon ), by = 'id') %>%
  group_by(col_type,data_type, id, season) %>%
  summarize(pred = mean(pred) 
            , espn_prob = mean(espn_prob)
            , winner = mean(winner %>% as.numeric() -1) 
            , arena_lat = mean(arena_lat)
            , arena_lon = mean(arena_lon)
  ) %>%
  mutate(pred = ifelse(pred >= .5,1,0)
         , espn_prob = ifelse(espn_prob >= 50,1,0)
         , pred_espn_prob = round((pred+espn_prob)/2)
  ) %>%
  mutate(count = winner == pred ) %>% filter(count == FALSE)


density_map_winner <- ggmap(map, extent = "panel", maprange=FALSE) +
  geom_density2d(data = map_winner_tbl,aes(x = arena_lon, y = arena_lat)) +
  stat_density2d(data = map_winner_tbl,aes(x = arena_lon, y = arena_lat, fill = ..level.., alpha = ..level..),
                 size = 0.01, geom = 'point') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))+
  # ggtitle("Shoe Carnival Store Location Dispersion")+
  theme(plot.title = element_text(size=10))
density_map_loser <- ggmap(map, extent = "panel", maprange=FALSE) +
  geom_density2d(data = map_loser_tbl,aes(x = arena_lon, y = arena_lat)) +
  stat_density2d(data = map_loser_tbl,aes(x = arena_lon, y = arena_lat, fill = ..level.., alpha = ..level..),
                 size = 0.01, geom = 'point') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))+
  # ggtitle("Shoe Carnival Store Location Dispersion")+
  theme(plot.title = element_text(size=10))

# predictions based on geographics, should we consider some geographic variable in our model

# PCA visual
# VIP Visual
# Density Plot

# Year over Year accuracy | day of the week 

all_models_tbl %>%
  left_join(model_data_tbl %>% mutate(id = row_number()) %>%select(id, season), by = 'id') %>%
  group_by(col_type,data_type, id, season) %>%
  summarize(pred = mean(pred), espn_prob = mean(espn_prob), winner = mean(winner %>% as.numeric() -1)) %>%
  mutate(pred = ifelse(pred >= .5,1,0)
         , espn_prob = ifelse(espn_prob >= 50,1,0)
         , pred_espn_prob = round((pred+espn_prob)/2)
  ) %>%
  mutate(count = winner == pred ) %>%
  group_by(col_type, data_type, season) %>%
  summarize(pred = sum(count), total = n()) %>%
  mutate(prop = pred/total)  %>% View()

