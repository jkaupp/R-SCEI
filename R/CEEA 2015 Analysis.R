library(xlsx)
library(dplyr)
library(magrittr)
library(lubridate)
library(xtable)

# Read in the data
data<-read.xlsx("/Users/Jake/ownCloud/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 1)
names<-read.xlsx("/Users/Jake/ownCloud/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 2)

# Set the names of the data to the modified headers
data %<>%
  set_names(names$modified.header)

# Fix the duration to decimal minutes
old_time <- data$time %>% 
  ymd_hms 

new_time <- minute(old_time) + second(old_time)/60
data$time <- new_time

# Calculate mean completion time
data$time %>% 
  mean



