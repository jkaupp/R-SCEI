library(xlsx)
library(plyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(pander)
library(gdata)
library(ggplot2)
library(ggthemes)
library(knitr)
library(knitcitations)
library(likert)
library(stringi)
library(stringr)

# User defined function to split levels to fit in legend. ----
split_levels <- function(df,from,to){
  levels(df) <- gsub(from, to, levels(df))
  return(df)
}

# Set options for Markdown tables in the document ----
panderOptions('table.caption.prefix',"Table ")

# Set Knitcitations options ----
options("citation_format" = "pandoc")

# Read in the data ----
survey_data <- read.xlsx("/Users/Jake/ownCloud/Projects/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 1)
var_names <- read.xlsx("/Users/Jake/ownCloud/Projects/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 2)

# Set the names of the data to the modified headers ----
survey_data %<>%
  set_names(var_names$modified.header)

# Fix the duration to decimal minutes ----
old_time <- survey_data$time %>% 
  ymd_hms 

new_time <- minute(old_time) + second(old_time)/60
survey_data$time <- new_time

# Reorder factor levels in Demographics ----
# Q2
survey_data$teaching_duration %<>%
  reorder.factor(new.order = c("0-6 years","7-15 years","16-25 years","> 25 years"))

# Reorder factor levels in Construct 1 ----
# Q5
survey_data$enjoyment %<>%
  reorder.factor(new.order = c(1,5,4,2,3)) %>% 
  split_levels(.," ","\n")

survey_data$enjoyment %<>%
  split_levels(.," ","\n")

# Q6
survey_data$goals_transmission %<>%
  reorder.factor(new.order = c(2,4,5,3,1)) %>% 
  split_levels(.," ","\n")

survey_data$goals_nuturing %<>%
  reorder.factor(new.order = c(2,4,5,3,1)) %>% 
  split_levels(.," ","\n")

survey_data$goals_apprenticeship %<>%
  reorder.factor(new.order = c(2,4,5,3,1)) %>% 
  split_levels(.," ","\n")

survey_data$goals_learner_centered %<>%
  reorder.factor(new.order = c(2,4,5,3,1)) %>% 
  split_levels(.," ","\n")

survey_data$goals_social_change %<>%
  reorder.factor(new.order = c(2,4,5,3,1)) %>% 
  split_levels(.," ","\n")

# Q7
survey_data$changes_personal_observation %<>%
  reorder.factor(new.order = c(1,3,4,2)) %>% 
  split_levels(.," ","\n")

survey_data$changes_colleague_input %<>%
  reorder.factor(new.order = c(1,3,4,2)) %>% 
  split_levels(.," ","\n")

survey_data$changes_course_evaluations %<>%
  reorder.factor(new.order = c(1,3,4,2)) %>% 
  split_levels(.," ","\n")

survey_data$changes_literature%<>%
  reorder.factor(new.order = c(1,3,4,2)) %>% 
  split_levels(.," ","\n")

survey_data$changes_pd%<>%
  reorder.factor(new.order = c(1,3,4,2)) %>% 
  split_levels(.," ","\n")

# Q8
survey_data$poor_course_administration %<>% 
  as.numeric

# Reorder factor levels in Construct 2 ----
# Q10
survey_data$instructor_content  %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

survey_data$instructor_teaching_practises %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

survey_data$instructor_motivate_and_provide %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

# Q11  
survey_data$student_ability_no_responsbility %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

survey_data$student_responsible_background_motivation %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

survey_data$student_attend_classes %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

# Q12
survey_data$methods_supporting_outcomes %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

survey_data$presentation_opportunity_management %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

survey_data$content_clarity %<>%
  reorder.factor(new.order = c(1,3,2)) %>% 
  split_levels(.," ","\n")

#Q14+15
survey_data$knowledge_expert %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$teaching_expert %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

levels(survey_data$teaching_expert) <- mapLevels(survey_data$knowledge_expert)

# Reorder factor levels in Construct 3 ----

#Q18
survey_data$teaching_skills_development  %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$teaching_processes  %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$teaching_assessment_methods %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$scholarship  %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

#Q19
survey_data$support_teaching_skills_development %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

survey_data$support_teaching_processes %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

survey_data$support_teaching_assessment_methods %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

survey_data$support_scholarship %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

#Q21
survey_data$obs_timing %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_availability %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_awareness %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_relevance %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_workload %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_lack_funding %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_lack_expertise %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

survey_data$obs_specificity %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8)) %>% 
  split_levels(.," ","\n")

levels(survey_data$obs_availability) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_awareness) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_relevance) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_workload) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_lack_funding) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_lack_expertise) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_specificity) <- mapLevels(survey_data$obs_timing)


#Q23

survey_data$nf_teaching_skills %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$nf_teaching_assessment_methods %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$nf_teaching_processes %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$nf_scholarship %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$ce_teaching_skills %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$ce_teaching_assessment_methods %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$ce_teaching_processes %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

survey_data$ce_scholarship %<>%
  reorder.factor(new.order = c(5,2,3,1,4)) %>% 
  split_levels(.," ","\n")

#Q24
survey_data$pd_in_pe %<>%
  reorder.factor(new.order = c(3,1,2))