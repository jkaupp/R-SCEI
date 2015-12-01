library(rio)
library(tidyr)
library(plyr)
library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)

SCEI.national.master <- import("data/SCEI National Raw Data.xlsx", sheet="National")
SCEI.codebook <- import("data/SCEI National Raw Data.xlsx", sheet="codebook")

scei_export <- function(df) {
  export(df, file = paste0(unique(df$Collector)," SCEI Institutional Data Set.xlsx"))
  zip(paste0(unique(df$Collector)," SCEI Package.zip"), 
      c("CEEA - SCEI National Data Set.xlsx","CEEA - SCEI Codebook.xlsx", paste0(unique(df$Collector)," SCEI Institutional Data Set.xlsx")))
}

# Get the original header names for non-text questions
o.header <- SCEI.codebook %>% 
  filter(type!="text") %>% 
  select(`original header`) %>% 
  mutate(`original header` = str_replace(`original header`, "\\t", ""))
  
# Export the National 
SCEI.national.master %>% 
  select(-contains("txt")) %>% 
  set_colnames(o.header$`original header`) %>% 
  mutate(`Completion Time` = round(minute(ymd_hms(`Completion Time`)) + second(ymd_hms(`Completion Time`))/60, 1),
         Collector = "National") %>% 
  .[sample(nrow(.)),] %>% 
  export("CEEA - SCEI National Data Set.xlsx")

# Export the codebook

SCEI.codebook %>% 
  export("CEEA - SCEI Codebook.xlsx")

# Export the Institutional datasets
SCEI.national.master %>% 
  select(-contains("txt")) %>% 
  set_colnames(o.header$`original header`) %>% 
  mutate(`Completion Time` = round(minute(ymd_hms(`Completion Time`)) + second(ymd_hms(`Completion Time`))/60, 1)) %>% 
  d_ply(.(Collector), scei_export)
  
  
  