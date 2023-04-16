if (!require('hoopR')) install.packages('hoopR')

mbb_box_score_2012_2022_tbl <- hoopR::load_mbb_team_box(seasons = 2012:2022)

readr::write_csv(mbb_box_score_2012_2022_tbl, "Data/raw/mbb_box_score_2012_2022_tbl.csv")