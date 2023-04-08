if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')

model_data_tbl <- readr::read_csv("Data/clean/model_data_tbl.csv")

model_data_tbl <- model_data_tbl %>%
  mutate(home_winner_response = as.numeric(home_winner))

set.seed(15) # for reproducibility
initial_split_data <- initial_split(model_data_tbl, prop = .6)
train_tbl <- training(initial_split_data)
test_temp_tbl <- testing(initial_split_data)

set.seed(15)
validation_split <- initial_split(test_temp_tbl, prop = .5)
test_tbl <- training(validation_split)
validation_tbl <- testing(validation_split)

rm(initial_split_data, validation_split, test_temp_tbl) # run as needed

DataExplorer::create_report(
  model_data_tbl
  , output_file = "model_data_tbl_eda"
  , output_dir = "Other Resources/"
  , y = "home_winner_response"
  , report_title = "EDA Report - MBB Home Winner"
  )
