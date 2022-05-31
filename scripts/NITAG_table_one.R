# This script makes a Table 1 for the cases and matched controls for the vaccine impact analyis
# It requires cc_matches to be run 1st

# Author: Chris
# Version: 2021-09-27

# Libraries
library(tidyverse)

# Parameters

# Parameters

sex_recode <-
  c(
    `1` = "Male",
    `2` = "Female"
  )

yes_no_recode <-
  c(
    `1` = "Yes",
    `0` = "No"
  )

hh_edu_recode <- c(
  `1` = "Illiterate",
  `2` = "Primary school certificate (through Grade 4)",
  `3` = "Middle school certificate (Grade 5 - 10)",
  `4` = "High school certificate (Grade 11 - 12)",
  `5` = "Intermediate or post high school diploma",
  `6` = "Graduate (Bachelors degree)",
  `7` = "Post graduate, profession, or honors"
)

hh_occu_recode <- c(
  `1` = "Legislators, Senior Officials & Managers",
  `2` = "Professionals (e.g. Doctors, Engineers, Bank managers)",
  `3` = "Technicians and Associate Professionals",
  `4` = "Clerks",
  `5` = "Skilled Workers and Shop & Market Sales Workers",
  `6` = "Skilled Agricultural & Fishery Workers",
  `7` = "Craft & Related Trade Workers",
  `8` = "Plant & Machine Operators and Assemblers",
  `9` = "Elementary Occupation (e.g. Domestic servant, peon, watchman)",
  `10` = "Mathadi or quarry workers",
  `11` = "Unemployed",
  `88` = "Other",
  `99` = "Refused"
)

toilet_recode <- c(
  `1` = "No facility / open defecation",
  `2` = "Traditional pit latrine",
  `3` = "Sarvajanik toilet (public toilets on the streets)",
  `4` = "Flush toilet",
  `88` = "Other",
  `99` = "Refused"
)

child_schooling_recode <- c(
  `1` = "Yes",
  `2` = "Not of school age",
  `3` = "Never attended school",
  `99` = "DK/Refused"
)
child_schooling_level_recode <- c(
  `1` = "Illiterate",
  `2` = "Primary school certificate (through Grade 4)",
  `3` = "Middle school certificate (Grade 5 - 10)",
  `4` = "High school certificate (Grade 11 - 12)",
  `99` ="Declined to answer"
)
yes_no_school_recode <-
  c(
    `1` = "Yes",
    `0` = "No",
    `2` = "Child doesn’t attend school"
  )

vars_interest <-
  c("sex_respondent", "age_30_39", "oc_non_prof", "ed_status_resp", "sex_child", "child_age_gt5")
#===============================================================================
# Code
cc_match_table_data <-
  cc_matches_case %>%
  mutate(
    sex_respondent = recode(sexresp, !!!sex_recode ),
    age_30_39 = if_else(agerel > 30 & agerel < 39, "30-39", "Not 30-39"),
    oc_non_prof = if_else(profess == 1, "Professional", "Non-Professional"),
    ed_status_resp = if_else(edu > 3, "Higher secondary or more", "Less than higher secondary"),
    sex_child = recode(sex, !!!sex_recode ),
    child_age_gt5 = if_else(AGEYR > 4, "5-16", "< 5"),
  )



tab_1 <-
  CreateTableOne(
  vars = vars_interest,
  data = cc_match_table_data,
  #factorVars = hh_cat_vars,
  strata = "case"
  )

tab_1 %>%
  print() %>%
  write.csv(file = "/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/table_one_NITAG_210927.csv")
