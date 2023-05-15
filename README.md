Team 15 MGT Madness (Gold Card Winner: Placed Top 7 of 90 Projects)
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
    ## 26  ¦--raw                                  
    ## 27  ¦   ¦--ap_poll_2012_2022_raw_tbl.csv    
    ## 28  ¦   ¦--geocoded_locations_tbl.csv       
    ## 29  ¦   ¦--mbb_attendance_2012_2022_tbl.csv 
    ## 30  ¦   °--mbb_box_score_2012_2022_tbl.csv  
    ## 31  °--sessionInfo.txt

    ##                                 levelName
    ## 1  Final Code                            
    ## 2   ¦--1. Data Gathering & Cleaning      
    ## 3   ¦   ¦--0. Package Check.R            
    ## 4   ¦   ¦--1. data_gathering_mbb_box.R   
    ## 5   ¦   ¦--2. data_gathering_attendance.R
    ## 6   ¦   ¦--3. data_gathering_ap_polls.R  
    ## 7   ¦   ¦--4. data_cleanup.R             
    ## 8   ¦   °--5. create_model_data.R        
    ## 9   ¦--2. Model & Visualization          
    ## 10  ¦   ¦--1. model_building.R           
    ## 11  ¦   ¦--2. model_predictions.R        
    ## 12  ¦   °--draw_confusion_matrix.R       
    ## 13  °--Team15FinalReport.Rmd

## Data Gathering & Cleaning

Within this directory there are 5 R scripts that were finalized to
gather and clean each of our data sources. These scripts utilize
numerous data sources such as:

1.  ESPN web API
2.  `hoopR::load_mbb_team_box()` function
3.  <https://www.sports-reference.com/> AP poll data
4.  Google Maps API to geocode arena information

Below are a few examples of our data gathering and cleaning process.

### Loading Packages

Most scripts will have packages listed at the top and should install or
load depending on your situation. If has to be installed, you will need
to run the line once more when the install completes in order to load
the package. Refer to the `0. Package Check.R` if there are any
concerns.

``` r
if (!require('devtools')) install.packages('devtools')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('tidymodels')) install.packages('tidymodels')
if (!require('tictoc')) install.packages('tictoc')
if (!require('hoopR')) devtools::install_github('sportsdataverse/hoopR')
if (!require('glue')) install.packages('glue')
if (!require('httr')) install.packages('httr')
if (!require('rvest')) install.packages('rvest')
if (!require('here')) install.packages('here')
if (!require('ggmap')) install.packages('ggmap')
if (!require('glmnet')) install.packages('glmnet')
if (!require('vip')) install.packages('vip')
if (!require('caret')) install.packages('caret')
if (!require('xgboost')) install.packages('xgboost')
if (!require('ggcorrplot')) install.packages('ggcorrplot')
if (!require('zoo')) install.packages('zoo')
if (!require('lubridate')) install.packages('lubridate')
if (!require('geosphere')) install.packages('geosphere')
if (!require('ggthemes')) install.packages('ggthemes')
if (!require('forcats')) install.packages('forcats')
if(!require('bookdown')) install.packages('bookdown')
if(!require('kableExtra')) install.packages('kableExtra')
if(!require('doParallel')) install.packages('doParallel')
if(!require('scales')) install.packages('scales')
if(!require('stringr')) install.packages('stringr')
require(parallel)
```

### Men’s Basketball Boxscore

This code will use the hoopR package and tidyverse to quickly collect
the 2022 box score data for all teams.

``` r
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

### ESPN Attendance Data

This data requires access to the ESPN API. This access is free but will
be throttled depending on use. If the script returns no data or missing
data then a VPN may be required to bypass the ESPN restrictions. This
function can be modified to include box score data similar to the
`hoopR::load_mbb_team_box()` function. Using the hoopR function saved us
time for this project but pulling directly from the API had many uses as
the data is much more likely to be completely filled in.

``` r
# Select First 5 Game IDs from our tibble
game_ids_vec <- mbb_box_score_2012_2022_tbl %>%
  pull(game_id) %>%
  unique() %>%
  .[1:5]

# Initialize ESPN Attendance function
get_attendance_espn_api <- function(game_id){

  tryCatch({url = glue::glue("https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary?event={game_id}")
    
    txt = httr::GET(url) %>% httr::content(as = "text")
    
    game_info <- jsonlite::fromJSON(txt)
    temp <- game_info %>% 
      enframe() %>% 
      pivot_wider() %>% 
      select(gameInfo, predictor, odds) %>%  
      unnest_wider(gameInfo) %>% 
      unnest_wider(venue) %>% 
      unnest_wider(predictor) %>%
      unnest_wider(homeTeam,names_sep = "_") %>% 
      unnest_wider(awayTeam,names_sep = "_") %>% 
      unnest_wider(address) %>% 
      unnest_wider(odds) %>%
      select(-officials, -images, -grass, -id, -header) %>%
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
    temp <- data.frame(game_id = game_id, type = 0)
    return(temp)
  })
}

tictoc::tic()
mbb_attendance_2012_2022_tbl <- lapply(game_ids_vec, get_attendance_espn_api) %>% bind_rows()
tictoc::toc()
```

    ## 2.05 sec elapsed

``` r
mbb_attendance_2012_2022_tbl
```

    ## # A tibble: 5 × 16
    ##   fullName   city  state capac…¹ atten…² homeT…³ homeT…⁴ homeT…⁵ homeT…⁶ awayT…⁷
    ##   <chr>      <chr> <chr>   <int>   <int> <chr>   <chr>   <chr>   <chr>   <chr>  
    ## 1 McCarthey… Spok… WA       6000    6000 2250    98.8    1.2     0.0     3101   
    ## 2 Pauley Pa… Los … CA      13800    5618 26      95.0    5.0     0.0     2934   
    ## 3 Madison S… New … NY      19812       0 127     35.5    64.5    0.0     2305   
    ## 4 Finneran … Vill… PA       6501    6501 222     98.3    1.7     0.0     116    
    ## 5 Frank Erw… Aust… TX      16540   14683 251     96.9    3.1     0.0     2277   
    ## # … with 6 more variables: awayTeam_gameProjection <chr>,
    ## #   awayTeam_teamChanceLoss <chr>, awayTeam_teamChanceTie <chr>, game_id <int>,
    ## #   type <dbl>, neutral_site <lgl>, and abbreviated variable names ¹​capacity,
    ## #   ²​attendance, ³​homeTeam_id, ⁴​homeTeam_gameProjection,
    ## #   ⁵​homeTeam_teamChanceLoss, ⁶​homeTeam_teamChanceTie, ⁷​awayTeam_id

## Model & Visualization

This directory contains 3 scripts in total:

1.  model_building.R
2.  model_predictions.R
3.  draw_confusion_matrix.R

Model building should be run first in order to create the models,
assuming you’ve already create/pulled all relevant data sources. Model
predictions will access our stored models and create the relevant R
objects for the RMarkdown file. The confusion matrix script is meant to
be a helper script that adds a bit of flare to our contingency tables.

### Model Building

We will not cover the models in detail but the general concept here is
to read in the data created in the previous steps then perform various
modeling techniques ranging from LASSO regression, Logistic Regression,
Probit Regression and Decision Trees. Please see the tidymodels
documentation to find out more about our approach and see examples. You
can find this documentation at <https://www.tidymodels.org/>

### Model Predictions

Again, we will not cover the model information in detail as the code is
quite long. Once again the concept is similar to Model Building. If you
have the models created or pulled from the repository, this script can
be ran and our contingency tables created for evaluation.

## Wrap up

Below is the session information that was used to create this report.
The packages and their versions are listed. R 4.2.3 is required.

``` r
sessionInfo()
```

    ## R version 4.2.3 (2023-03-15 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 22621)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] doParallel_1.0.17  iterators_1.0.14   foreach_1.5.2      kableExtra_1.3.4  
    ##  [5] bookdown_0.29      ggthemes_4.2.4     geosphere_1.5-18   lubridate_1.9.0   
    ##  [9] timechange_0.1.1   zoo_1.8-10         ggcorrplot_0.1.4   xgboost_1.7.3.1   
    ## [13] caret_6.0-93       lattice_0.20-45    vip_0.3.2          glmnet_4.1-4      
    ## [17] Matrix_1.5-1       ggmap_3.0.1        here_1.0.1         rvest_1.0.3       
    ## [21] httr_1.4.5         glue_1.6.2         hoopR_1.9.1.9000   tictoc_1.1        
    ## [25] yardstick_1.1.0    workflowsets_1.0.0 workflows_1.1.0    tune_1.0.0        
    ## [29] rsample_1.1.0      recipes_1.0.1      parsnip_1.0.1      modeldata_1.0.1   
    ## [33] infer_1.0.3        dials_1.0.0        scales_1.2.1       broom_1.0.1       
    ## [37] tidymodels_1.0.0   devtools_2.4.5     usethis_2.1.6      plyr_1.8.7        
    ## [41] data.tree_1.0.0    forcats_0.5.2      stringr_1.5.0      dplyr_1.0.10      
    ## [45] purrr_1.0.1        readr_2.1.2        tidyr_1.3.0        tibble_3.1.8      
    ## [49] ggplot2_3.3.6      tidyverse_1.3.2   
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] readxl_1.4.1         backports_1.4.1      systemfonts_1.0.4   
    ##   [4] selectr_0.4-2        sp_1.6-0             splines_4.2.3       
    ##   [7] listenv_0.9.0        digest_0.6.29        htmltools_0.5.3     
    ##  [10] fansi_1.0.3          magrittr_2.0.3       memoise_2.0.1       
    ##  [13] googlesheets4_1.0.1  tzdb_0.3.0           remotes_2.4.2       
    ##  [16] globals_0.16.2       modelr_0.1.9         gower_1.0.0         
    ##  [19] RcppParallel_5.1.5   svglite_2.1.1        hardhat_1.2.0       
    ##  [22] prettyunits_1.1.1    jpeg_0.1-10          colorspace_2.0-3    
    ##  [25] haven_2.5.1          xfun_0.33            callr_3.7.2         
    ##  [28] crayon_1.5.2         jsonlite_1.8.0       progressr_0.13.0    
    ##  [31] survival_3.5-3       gtable_0.3.1         gargle_1.2.1        
    ##  [34] ipred_0.9-13         webshot_0.5.4        pkgbuild_1.3.1      
    ##  [37] shape_1.4.6          future.apply_1.9.1   DBI_1.1.3           
    ##  [40] miniUI_0.1.1.1       Rcpp_1.0.9           viridisLite_0.4.1   
    ##  [43] xtable_1.8-4         GPfit_1.0-8          stats4_4.2.3        
    ##  [46] lava_1.6.10          prodlim_2019.11.13   profvis_0.3.7       
    ##  [49] htmlwidgets_1.5.4    ellipsis_0.3.2       urlchecker_1.0.1    
    ##  [52] pkgconfig_2.0.3      nnet_7.3-18          dbplyr_2.2.1        
    ##  [55] utf8_1.2.2           janitor_2.2.0        reshape2_1.4.4      
    ##  [58] tidyselect_1.2.0     rlang_1.0.6          DiceDesign_1.9      
    ##  [61] later_1.3.0          munsell_0.5.0        cellranger_1.1.0    
    ##  [64] tools_4.2.3          cachem_1.0.6         cli_3.4.0           
    ##  [67] generics_0.1.3       evaluate_0.16        fastmap_1.1.0       
    ##  [70] yaml_2.3.5           ModelMetrics_1.2.2.2 processx_3.7.0      
    ##  [73] knitr_1.40           fs_1.5.2             RgoogleMaps_1.4.5.3 
    ##  [76] nlme_3.1-162         future_1.32.0        mime_0.12           
    ##  [79] xml2_1.3.3           compiler_4.2.3       rstudioapi_0.14     
    ##  [82] curl_4.3.2           png_0.1-7            reprex_2.0.2        
    ##  [85] lhs_1.1.5            stringi_1.7.8        ps_1.7.1            
    ##  [88] vctrs_0.5.2          pillar_1.8.1         lifecycle_1.0.3     
    ##  [91] furrr_0.3.1          bitops_1.0-7         data.table_1.14.2   
    ##  [94] httpuv_1.6.6         R6_2.5.1             promises_1.2.0.1    
    ##  [97] gridExtra_2.3        parallelly_1.34.0    sessioninfo_1.2.2   
    ## [100] codetools_0.2-19     MASS_7.3-58.2        assertthat_0.2.1    
    ## [103] pkgload_1.3.0        rprojroot_2.0.3      withr_2.5.0         
    ## [106] hms_1.1.2            grid_4.2.3           rpart_4.1.19        
    ## [109] timeDate_4021.104    class_7.3-21         rmarkdown_2.16      
    ## [112] snakecase_0.11.0     googledrive_2.0.0    pROC_1.18.0         
    ## [115] shiny_1.7.2
