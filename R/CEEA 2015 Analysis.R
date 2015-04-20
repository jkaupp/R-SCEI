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
library(likert)
library(stringi)
library(stringr)

# Read in the data
survey_data <- read.xlsx("/Users/Jake/ownCloud/Projects/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 1)
var_names <- read.xlsx("/Users/Jake/ownCloud/Projects/R/Projects/R-SCEI/data/iterim_national_data.xlsx", 2)


# var_names$modified.header %<>%
#   paste0(var_names$question,"-",.)

# Set the names of the data to the modified headers
survey_data %<>%
  set_names(var_names$modified.header)

# Fix the duration to decimal minutes
old_time <- survey_data$time %>% 
  ymd_hms 

new_time <- minute(old_time) + second(old_time)/60
survey_data$time <- new_time

# Reorder factor levels appropriate for display purposes
survey_data$teaching_duration %<>%
  reorder.factor(new.order = c("0-6 years","7-15 years","16-25 years","> 25 years"))

survey_data$enjoyment %<>%
  reorder.factor(new.order = c(1,5,4,2,3))

survey_data$goals_transmission %<>%
  reorder.factor(new.order = c(2,4,5,3,1))

survey_data$goals_nuturing %<>%
  reorder.factor(new.order = c(2,4,5,3,1))

survey_data$goals_apprenticeship %<>%
  reorder.factor(new.order = c(2,4,5,3,1))

survey_data$goals_learner_centered %<>%
  reorder.factor(new.order = c(2,4,5,3,1))

survey_data$goals_social_change %<>%
  reorder.factor(new.order = c(2,4,5,3,1))

survey_data$goals_other %<>%  
  reorder.factor(new.order = c(2,4,5,3,1))
levels(survey_data$goals_other) <- c("First priority","Second priority","Third priority","Fourth priority","Fifth priority" ) 

survey_data$changes_personal_observation %<>%
  reorder.factor(new.order = c(1,3,4,2))

survey_data$changes_colleague_input %<>%
  reorder.factor(new.order = c(1,3,4,2))

survey_data$changes_course_evaluations %<>%
  reorder.factor(new.order = c(1,3,4,2))

survey_data$changes_literature%<>%
  reorder.factor(new.order = c(1,3,4,2))

survey_data$changes_pd%<>%
  reorder.factor(new.order = c(1,3,4,2))

survey_data$poor_course_administration %<>% 
  as.numeric

survey_data$instructor_content  %<>%
  reorder.factor(new.order = c(1,3,2))

survey_data$instructor_teaching_practises %<>%
  reorder.factor(new.order = c(1,3,2))

survey_data$instructor_motivate_and_provide %<>%
  reorder.factor(new.order = c(1,3,2))
  
survey_data$student_ability_no_responsbility %<>%
  reorder.factor(new.order = c(1,3,2))

survey_data$student_responsible_background_motivation %<>%
  reorder.factor(new.order = c(1,3,2))

survey_data$student_attend_classes %<>%
  reorder.factor(new.order = c(1,3,2))

# Q12
survey_data$methods_supporting_outcomes %<>%
  reorder.factor(new.order = c(1,3,2))

survey_data$presentation_opportunity_management %<>%
  reorder.factor(new.order = c(1,3,2))

survey_data$content_clarity %<>%
  reorder.factor(new.order = c(1,3,2))

#Q14+15
survey_data$knowledge_expert %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

levels(survey_data$teaching_expert) <- mapLevels(survey_data$knowledge_expert)

#Q18
survey_data$teaching_skills_development  %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

levels(survey_data$teaching_processes) <- mapLevels(survey_data$teaching_skills_development)
levels(survey_data$teaching_assessment_methods) <- mapLevels(survey_data$teaching_skills_development)
levels(survey_data$scholarship) <- mapLevels(survey_data$teaching_skills_development)

#Q19
survey_data$support_teaching_skills_development %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

levels(survey_data$teaching_processes) <- mapLevels(survey_data$teaching_skills_development)
levels(survey_data$teaching_assessment_methods) <- mapLevels(survey_data$teaching_skills_development)
levels(survey_data$scholarship) <- mapLevels(survey_data$teaching_skills_development)

#Q21

survey_data$obs_timing %<>%
  reorder.factor(new.order = c(9,1,2,3,4,5,6,7,8))

levels(survey_data$obs_availability) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_awareness) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_relevance) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_workload) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_lack_funding) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_lack_expertise) <- mapLevels(survey_data$obs_timing)
levels(survey_data$obs_specificity) <- mapLevels(survey_data$obs_timing)

#Q23

survey_data$nf_teaching_skills %<>%
  reorder.factor(new.order = c(5,2,3,1,4))

levels(survey_data$nf_teaching_assessment_methods) <- mapLevels(survey_data$nf_teaching_skills)
levels(survey_data$nf_teaching_processes) <- mapLevels(survey_data$nf_teaching_skills)
levels(survey_data$nf_scholarship) <- mapLevels(survey_data$nf_teaching_skills)

levels(survey_data$ce_teaching_skills) <- mapLevels(survey_data$nf_teaching_skills)
levels(survey_data$ce_teaching_assessment_methods) <- mapLevels(survey_data$nf_teaching_skills)
levels(survey_data$ce_teaching_processes) <- mapLevels(survey_data$nf_teaching_skills)
levels(survey_data$ce_scholarship) <- mapLevels(survey_data$nf_teaching_skills)

#Q24

survey_data$pd_in_pe %<>%
  reorder.factor(new.order = c(3,1,2))


# Set options for Markdown tables in the document
panderOptions('table.caption.prefix',"Table ")

# Assignment to provide common plot theme
# Future work: Create a theme for all SCEI plots

scei.theme <- theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"))
            
 

