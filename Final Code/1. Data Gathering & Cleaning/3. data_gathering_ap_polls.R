if (!require('rvest')) install.packages('rvest')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('glue')) install.packages('glue')
get_ap_polls <- function(year){
  print(year)
  url <- glue::glue("https://www.sports-reference.com/cbb/seasons/men/{year}-polls.html#ap-polls")
  
  webpage <- read_html(url)
  
  data <- html_table(html_nodes(webpage,"table")[1])[[1]]
  names(data) <- c("school","conference",1:ncol(data))
  data <- data %>%
    mutate(year = year)
  
  return(data)
  
}

ap_poll_2012_2022_tbl <- lapply(2012:2022, get_ap_polls) %>% bind_rows()

readr::write_csv(ap_poll_2012_2022_tbl,"Data/raw/ap_poll_2012_2022_raw_tbl.csv")
