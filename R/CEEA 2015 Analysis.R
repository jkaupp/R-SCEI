library(xlsx)
library(dplyr)
library(magrittr)
library(lubridate)
library(pander)

# Read in the data
survey_data<-read.xlsx("/Users/Jake/ownCloud/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 1)
var_names<-read.xlsx("/Users/Jake/ownCloud/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 2)

# Set the names of the data to the modified headers
survey_data %<>%
  set_names(var_names$modified.header)

# Fix the duration to decimal minutes
old_time <- survey_data$time %>% 
  ymd_hms 

new_time <- minute(old_time) + second(old_time)/60
survey_data$time <- new_time

survey_data$collector %>% 
  table %>% 
  as.data.frame %>%
  set_names(c("Institution","Responses")) %>% 
  pander


