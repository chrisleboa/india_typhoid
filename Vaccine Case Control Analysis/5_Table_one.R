# Table One numbers for Typhoid Effect paper using the table one categories provided by Qian at the CDC
# Using the dataset cc_matches_tcv_vax from the 2_Vaccine_Effectiveness_analysis dataset

# Author: Chris LeBoa
# Version: 2023-07-15

# Libraries
library(tidyverse)
library(tableone)

# Parameters

glimpse(cc_matches_tcv_vax)

profes_recode



cc_matches_tcv_vax_edit <-
cc_matches_tcv_vax %>%
  mutate(
    resp_sex = if_else(ve_sexresp == 1, "Male", "Female"),
    hh_size = if_else(numhh %in% c("3","4"), "1", "0"),
    resp_profess = if_else(ve_ocu <= 3, "Professional", "Non-professional"),
    resp_edu = if_else(ve_edu <=3, "Middle school or less", "Above Middle School"),
    resp_age_group = if_else(ve_agerel <=39 & ve_agerel >=30, "1", "0"),
    ch_age_group = if_else(age_mo_shot >= 60, "Age 5-16", "Younger than 5"),
    toilet_share = if_else(ses_toiletshare == 1, "Share", "No Share"),
    toilet_flush = if_else(ses_toitype == 4, "Flush", "No Flush"),
    handwash = if_else(ses_hwplace == 1, "Handwash", "No Handwash"),
    elec = if_else(ses_hhitems_a ==1, "electricity", "no electricity"),
    water_pro = if_else(ses_watsrc %in% c(1, 2, 3, 4, 5, 7), "improved", "not improved"),
    water_treat = if_else(ses_watsafe == 1, "Treat", "not treat")
  )

list_variables <-
  c("sex", "ch_age_group", "resp_age_group", "ve_sexresp", "resp_profess", "resp_edu", "hh_size","toilet_share", "toilet_flush", "handwash", "elec", "water_pro", "water_treat")
list_catvar <- c("ch_age_group","resp_profess", "resp_age_group","resp_edu", "ve_sexresp","sex", "hh_size","toilet_share", "toilet_flush", "handwash","elec", "water_pro", "water_treat")

#===============================================================================

tableOne <-
  CreateTableOne(
    data = cc_matches_tcv_vax_edit,
    vars = list_variables,
    factorVars = list_catvar,
    strata = "case"
  )

tableOne
