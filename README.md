Team 15 MGT Madness
================

The goal of MGT Madness was to create a machine learning model that
reliably predicted the outcome of both March Madness and regular season
college basketball games at a level equal or better to ESPN’s
industry-standard prediction models.

## Github File Structure

If you plan to use this repository ensure that you have the following
folders stored in the same directory as your .Rproj file. These
directories are required to ensure each script runs. Make sure to
include each of the .csv and/or .rds data files.

    ##                                    levelName
    ## 1  Data                                     
    ## 2   ¦--clean                                
    ## 3   ¦   ¦--ap_poll_clean_tbl.csv            
    ## 4   ¦   ¦--away_team_stats_tbl.csv          
    ## 5   ¦   ¦--date_tbl.csv                     
    ## 6   ¦   ¦--game_information_tbl.csv         
    ## 7   ¦   ¦--home_team_stats_tbl.csv          
    ## 8   ¦   ¦--model_data_tbl.csv               
    ## 9   ¦   °--model_data_tbl.zip               
    ## 10  ¦--models                               
    ## 11  ¦   ¦--baseline_rolling_logit_model.rds 
    ## 12  ¦   ¦--baseline_season_logit_model.rds  
    ## 13  ¦   ¦--baseline_standard_logit_model.rds
    ## 14  ¦   ¦--enhanced_rolling_logit_model.rds 
    ## 15  ¦   ¦--enhanced_rolling_xgb_model.rds   
    ## 16  ¦   ¦--enhanced_season_logit_model.rds  
    ## 17  ¦   ¦--enhanced_season_xgb_model.rds    
    ## 18  ¦   ¦--enhanced_standard_logit_model.rds
    ## 19  ¦   ¦--enhanced_standard_xgb_model.rds  
    ## 20  ¦   ¦--simple_rolling_logit_model.rds   
    ## 21  ¦   ¦--simple_rolling_probit_model.rds  
    ## 22  ¦   ¦--simple_season_logit_model.rds    
    ## 23  ¦   ¦--simple_season_probit_model.rds   
    ## 24  ¦   ¦--simple_standard_logit_model.rds  
    ## 25  ¦   °--simple_standard_probit_model.rds 
    ## 26  °--raw                                  
    ## 27      ¦--ap_poll_2012_2022_raw_tbl.csv    
    ## 28      ¦--geocoded_locations_tbl.csv       
    ## 29      ¦--mbb_attendance_2012_2022_tbl.csv 
    ## 30      °--mbb_box_score_2012_2022_tbl.csv

    ##                                 levelName
    ## 1  Final Code                            
    ## 2   ¦--1. Data Gathering & Cleaning      
    ## 3   ¦   ¦--1. data_gathering_mbb_box.R   
    ## 4   ¦   ¦--2. data_gathering_attendance.R
    ## 5   ¦   ¦--3. data_gathering_ap_polls.R  
    ## 6   ¦   ¦--4. data_cleanup.R             
    ## 7   ¦   °--5. create_model_data.R        
    ## 8   °--2. Model & Visualization          
    ## 9       ¦--1. model_building.R           
    ## 10      ¦--2. model_predictions.R        
    ## 11      °--draw_confusion_matrix.R

## Data Gathering & Cleaning

Within this directory there are 5 R scripts that were finalized to
gather and clean each of our data sources. These scripts utilize
numerous data sources such as:

1.  ESPN web API
2.  `hoopR::load_mbb_team_box()` function
3.  <https://www.sports-reference.com/> AP poll data
4.  Google Maps API to geocode arena information

Below are a few examples of our data gathering and cleaning process.

### Men’s Basketball Boxscore

This code will use the hoopR package and tidyverse to quickly collect
the 2022 box score data for all teams.

``` r
if (!require('hoopR')) install.packages('hoopR')
if (!require('tidyverse')) install.packages('tidyverse')
mbb_box_score_2012_2022_tbl <- hoopR::load_mbb_team_box(seasons = 2022)

mbb_box_score_2012_2022_tbl %>% head()
```

    ## # A tibble: 6 × 41
    ##   team_id team_uid       team_…¹ team_…² team_…³ team_…⁴ team_…⁵ team_…⁶ team_…⁷
    ##   <chr>   <chr>          <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ## 1 3101    s:40~l:41~t:3… utah-t… Utah T… Trailb… UTU     Utah T… Utah T… 000000 
    ## 2 2250    s:40~l:41~t:2… gonzag… Gonzaga Bulldo… GONZ    Gonzag… Gonzaga 002967 
    ## 3 2934    s:40~l:41~t:2… cal-st… Cal St… Roadru… CSUB    Cal St… Bakers… 003BAB 
    ## 4 26      s:40~l:41~t:26 ucla-b… UCLA    Bruins  UCLA    UCLA B… UCLA    005C8E 
    ## 5 2305    s:40~l:41~t:2… kansas… Kansas  Jayhaw… KU      Kansas… Kansas  0022B4 
    ## 6 127     s:40~l:41~t:1… michig… Michig… Sparta… MSU     Michig… Michig… 18453B 
    ## # … with 32 more variables: team_logo <chr>, team_alternate_color <chr>,
    ## #   field_goals_made_field_goals_attempted <chr>, field_goal_pct <chr>,
    ## #   three_point_field_goals_made_three_point_field_goals_attempted <chr>,
    ## #   three_point_field_goal_pct <chr>,
    ## #   free_throws_made_free_throws_attempted <chr>, free_throw_pct <chr>,
    ## #   total_rebounds <chr>, offensive_rebounds <chr>, defensive_rebounds <chr>,
    ## #   team_rebounds <chr>, assists <chr>, steals <chr>, blocks <chr>, …

### AP Poll Data

This function was built to scrape the AP poll data from
sports-reference. Both the rvest and tidyverse suite of packages were
used. Additional data cleaning was performed in our `data_cleanup.R`
script.

``` r
if (!require('rvest')) install.packages('rvest')
if (!require('tidyverse')) install.packages('tidyverse')

get_ap_polls <- function(year){

  url <- glue::glue("https://www.sports-reference.com/cbb/seasons/men/{year}-polls.html#ap-polls")
  
  webpage <- read_html(url)
  
  data <- html_table(html_nodes(webpage,"table")[1])[[1]]
  names(data) <- c("school","conference",1:ncol(data))
  data <- data %>%
    mutate(year = year)
  
  return(data)
  
}

ap_poll_2012_2022_tbl <- lapply(2012:2022, get_ap_polls) %>% bind_rows()

ap_poll_2012_2022_tbl
```

    ## # A tibble: 544 × 23
    ##    school    confe…¹ `1`   `2`   `3`   `4`   `5`   `6`   `7`   `8`   `9`   `10` 
    ##    <chr>     <chr>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ##  1 ""        ""      "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"   "10" 
    ##  2 "School"  "Conf"  "Pre" "11/… "11/… "11/… "12/… "12/… "12/… "12/… "1/2" "1/9"
    ##  3 "Alabama" "SEC"   "19"  "16"  "13"  "12"  "16"  "23"  ""    ""    ""    ""   
    ##  4 "Arizona" "Pac-1… "16"  "15"  "23"  ""    ""    ""    ""    ""    ""    ""   
    ##  5 "Baylor"  "Big 1… "12"  "11"  "9"   "7"   "6"   "6"   "6"   "6"   "4"   "4"  
    ##  6 "Califor… "Pac-1… "24"  "23"  "20"  "24"  ""    ""    ""    ""    ""    ""   
    ##  7 "Cincinn… "Big E… "21"  "20"  ""    ""    ""    ""    ""    ""    ""    ""   
    ##  8 "Creight… "MVC"   ""    ""    ""    ""    "19"  "25"  "23"  "21"  ""    "23" 
    ##  9 "Duke"    "ACC"   "6"   "6"   "6"   "3"   "7"   "7"   "7"   "7"   "5"   "8"  
    ## 10 "Florida" "SEC"   "8"   "7"   "10"  "10"  "12"  "13"  "11"  "10"  "13"  "19" 
    ## # … with 534 more rows, 11 more variables: `11` <chr>, `12` <chr>, `13` <chr>,
    ## #   `14` <chr>, `15` <chr>, `16` <chr>, `17` <chr>, `18` <chr>, `19` <chr>,
    ## #   year <int>, `20` <chr>, and abbreviated variable name ¹​conference
