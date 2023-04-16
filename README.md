Team 15 MGT Madness
================

The goal of MGT Madness was to create a machine learning model that
reliably predicted the outcome of both March Madness and regular season
college basketball games at a level equal or better to ESPN’s
industry-standard prediction models.

## Github File Structure

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
