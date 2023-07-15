# Case control analysis Update

# Author: Chris LeBoa
# Version: 2023-03-17

# Libraries
#install.packages("MatchIt")
library(tidyverse)
library(haven)
library(lubridate)
library(fs)
library(tableone)
library(MatchIt)
library(survival)

data <- read_sas("/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/tcv_all__2021_04_18_vax_08_31.sas7bdat")
#===============================================================================

  #The paper draft says 4 controls per case pulled from the general population that had been asked questions in the study.
  # 1) had not experienced fever within 30 days prior to interview,
         #Filter by fev1mo == 0
  # 2) were interviewed within 14 days of the matched case detection
        #The clinical cases have a cc_started id. This must fall within 2 weeks of the variable "today"
        #The cases do not have this variable while the controls do.
         #To deal with this i am making a date enrolled variable (dt_enrolled) that will match across both using 14 days
  # 3) were between the age of 9 months and 14 years in July 2018 and

  # 4) whose age was within 12 months of the age of the matched case.
        #Use agemocalc = 12 variable

## I think that we should use the MatchIT package like in the other VE analysis

# Libraries
library(tidyverse)
library(haven)


data_location <- "/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/tcv_all__2021_04_18_vax_08_31.sas7bdat"
data_location_cc <- "/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/tcv_caco__2020_07_06.sas7bdat"
#dataset with all data in it up to March 2021

#===============================================================================

#Read in data
all_data <- read_sas(data_location)
case_control_data <- read_sas(data_location_cc)

ccs_data <- read_sas("/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/ccs_ch_2020_04_24_ver2.sas7bdat")

#The age also must be within 1 year of the control

cc_filter <-
case_control_data %>%
  mutate(
    case = if_else(case_case_id!="",1,0),
    dt_enrolled = if_else(!is.na(cc_completed_time), cc_completed_time, today),
    dt_enrolled = as_date(dt_enrolled),
    months_from_shot = interval(as_date("2018-07-01"), dt_enrolled) / months(1),
    age_mo_shot = agemocalc - months_from_shot,
    typhoid_campaign_card = case_when(
      anytyphoidvx == 1 ~ 1,
      anytyphoidvx == 0 ~ 0,
      .default = 99),
    typhoid_campaign_card_recall = case_when(
      anytyphoidvx == 1 ~ 1,
      anytyphoidvx == 2 ~ 1,
      anytyphoidvx == 0 ~ 0,
      .default = 99
    ),
    tcv_any = case_when(
      anytyphoidvx == 1 ~ 1,
      anytyphoidvx == 2 ~ 1,
      anytyphoidvx == 3 ~ 1,
      anytyphoidvx == 0 ~ 0,
      .default = 99
    ),
    typhoid_vax_any = case_when(  # This is to get any individual who has a typhoid vaccine on file except those with known polysaccharide vaccines
      anytyphoidvx == 1 ~ 1,
      anytyphoidvx == 2 ~ 1,
      anytyphoidvx == 3 ~ 1,
      anytyphoidvx == 5 ~ 1,
      anytyphoidvx == 0 ~ 0,
      .default = 99
    )
    ) %>%
  drop_na(agemocalc)

#filter to get the selection parameters
cc_data <-
  cc_filter %>%
  filter(
    dt_enrolled >= as_date("2018-07-01"),  #Date enrolled is after campaign
    age_mo_shot >= 9 & age_mo_shot <= (14*12), #Age is between 9 months and 14 at time of campaign
    fev1mo == 0 | is.na(fev1mo)          #there is no fever in last month
    )

#Confirming that all non cases are not missing a fever variable

### Calculating numbers for figure 1

cc_filter %>%
  count(case)

  cc_filter %>%
  filter(
    age_mo_shot >= 9 & age_mo_shot <= (14*12)) %>%
      count(case)

  cc_data %>%
    count(tcv_vax, case)

####### Creation of matched data
  # As we do not want to match based on those who have an unknown vaccine type (and are removing the unknown types from the dataset above)
  # This will create 4 separate analysis datasets below
      #1) TCV Vax Campaign
      #2) TCV Campaign or Recall
      #3) TCV vax  ### This is what is used for additional analyses like age separation
      #4) TCV any
      #4) Typhoid vax any
  #Each match done as a
     # 4:1 NN PS matching w/o replacement
     # Matching based on agemonthcalc of individual (12 months) and the dt enrolled variable
     # Unmatched cases removed from the dataset
          # Number matched as well as the Number unmatched recorded in csv document (if i could code better would do this automatically)

#1) TCV card only

cc_data_tcv_card <-
  cc_data %>%
  filter(typhoid_campaign_card != 99)

cc_data_tcv_card %>%
  count(case)

caco_results <-
  matchit(case ~ agemocalc + dt_enrolled , data = cc_data_tcv_card, caliper = c(agemocalc = 12, dt_enrolled = 14),
          std.caliper = c(FALSE, FALSE),
        #  method = "exact",
          distance = "glm",
          ratio = 4)

matches <-
  data.frame(treatment= row.names(caco_results$match.matrix), control=caco_results$match.matrix) %>%
  mutate(pair = row_number()) %>%
  pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
  mutate(
    cc_type = if_else(str_detect(type, "treatment") == TRUE, "case", "control")
  ) %>%
  drop_na(id)

#Creates new dataset arranged by id number
cc_matches_id <-
  cc_data_tcv_card %>% #Change this per dataset making
  mutate(row_num = as.character(row_number())) %>%
  #select(row_num) %>%
  left_join(matches, by = c("row_num" = "id")) %>%
  drop_na(pair) %>%
  arrange(pair, desc(styphi))

## Remove instances where there is no match to only retain matched cases and controls
cc_matches_tcv_card <-
  cc_matches_id %>%
  group_by(pair) %>%
  filter(n() != 1) %>%
  ungroup()

 plot(caco_results, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched


 cc_matches_tcv_card %>% #Print number matched cases and controls
   count(case)

 #2) TCV card or recall

 cc_data_tcv_card_recall <-
   cc_data %>%
   filter(typhoid_campaign_card_recall!= 99)

 cc_data_tcv_card_recall %>%
   count(case)

 caco_results <-
   matchit(case ~ agemocalc + dt_enrolled , data = cc_data_tcv_card_recall, caliper = c(agemocalc = 12, dt_enrolled = 14),
           std.caliper = c(FALSE, FALSE),
           #  method = "exact",
           distance = "glm",
           ratio = 4)

 matches <-
   data.frame(treatment= row.names(caco_results$match.matrix), control=caco_results$match.matrix) %>%
   mutate(pair = row_number()) %>%
   pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
   mutate(
     cc_type = if_else(str_detect(type, "treatment") == TRUE, "case", "control")
   ) %>%
   drop_na(id)

 #Creates new dataset arranged by id number
 cc_matches_id <-
   cc_data_tcv_card_recall %>%                    #Change this per dataset making
   mutate(row_num = as.character(row_number())) %>%
   #select(row_num) %>%
   left_join(matches, by = c("row_num" = "id")) %>%
   drop_na(pair) %>%
   arrange(pair, desc(styphi))

 ## Remove instances where there is no match to only retain matched cases and controls
 cc_matches_tcv_card_recall <-
   cc_matches_id %>%
   group_by(pair) %>%
   filter(n() != 1) %>%
   ungroup()

 plot(caco_results, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched


 cc_matches_tcv_card_recall %>% #Print number matched cases and controls
   count(case)

#This dataset is the same as analysis dataset #1 but the contols are selected from a less conservative pool

#3 This is the main dataset used for analysis
 cc_data_tcv_vax <-
   cc_data %>%
   filter(tcv_vax != 99)

 cc_data_tcv_vax %>%
   count(case)

 caco_results <-
   matchit(case ~ agemocalc + dt_enrolled , data = cc_data_tcv_vax, caliper = c(agemocalc = 12, dt_enrolled = 14),
           std.caliper = c(FALSE, FALSE),
           #  method = "exact",
           distance = "glm",
           ratio = 4)

 matches <-
   data.frame(treatment= row.names(caco_results$match.matrix), control=caco_results$match.matrix) %>%
   mutate(pair = row_number()) %>%
   pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
   mutate(
     cc_type = if_else(str_detect(type, "treatment") == TRUE, "case", "control")
   ) %>%
   drop_na(id)

 #Creates new dataset arranged by id number
 cc_matches_id <-
   cc_data_tcv_vax %>% #Change this per dataset making
   mutate(row_num = as.character(row_number())) %>%
   #select(row_num) %>%
   left_join(matches, by = c("row_num" = "id")) %>%
   drop_na(pair) %>%
   arrange(pair, desc(styphi))

 ## Remove instances where there is no match to only retain matched cases and controls
 cc_matches_tcv_vax <-
   cc_matches_id %>%
   group_by(pair) %>%
   filter(n() != 1) %>%
   ungroup()

 plot(caco_results, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched

 cc_matches_tcv_vax %>% #Print number matched cases and controls
   count(case, tcv_vax)




 #4 TCV any

 # The definition for this variable in the codebook is unclear and I feel unable to use it.
 cc_data_tcv_any <- #change
   cc_data %>%
   filter(tcv_any!= 99) #Change to keep up with vaccination status of interest

 cc_data_tcv_any %>%  #Change
   count(case)

 caco_results <-
   matchit(case ~ agemocalc + dt_enrolled , data = cc_data_tcv_any, caliper = c(agemocalc = 12, dt_enrolled = 14), #Change
           std.caliper = c(FALSE, FALSE),
           #  method = "exact",
           distance = "glm",
           ratio = 4)

 matches <-
   data.frame(treatment= row.names(caco_results$match.matrix), control=caco_results$match.matrix) %>%
   mutate(pair = row_number()) %>%
   pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
   mutate(
     cc_type = if_else(str_detect(type, "treatment") == TRUE, "case", "control")
   ) %>%
   drop_na(id)

 #Creates new dataset arranged by id number
 cc_matches_id <-
   cc_data_tcv_any %>%                    #Change this per dataset making
   mutate(row_num = as.character(row_number())) %>%
   #select(row_num) %>%
   left_join(matches, by = c("row_num" = "id")) %>%
   drop_na(pair) %>%
   arrange(pair, desc(styphi))

 ## Remove instances where there is no match to only retain matched cases and controls
 cc_matches_tcv_any <-  #change
   cc_matches_id %>%
   group_by(pair) %>%
   filter(n() != 1) %>%
   ungroup()

 plot(caco_results, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched


 cc_matches_tcv_any %>% #Print number matched cases and controls
   count(case)

 #5 Typhoid vax any
 cc_data_typhoid_vax_any <- #change
   cc_data %>%
   filter(typhoid_vax_any!= 99) #Change to keep up with vaccination status of interest

 cc_data_typhoid_vax_any %>%  #Change
   count(case)

 cc_data_typhoid_vax_any %>% count(case, typhoid_vax_any)

 caco_results <-
   matchit(case ~ agemocalc + dt_enrolled , data = cc_data_typhoid_vax_any, caliper = c(agemocalc = 12, dt_enrolled = 14), #Change
           std.caliper = c(FALSE, FALSE),
           #  method = "exact",
           distance = "glm",
           ratio = 4)

 matches <-
   data.frame(treatment= row.names(caco_results$match.matrix), control=caco_results$match.matrix) %>%
   mutate(pair = row_number()) %>%
   pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
   mutate(
     cc_type = if_else(str_detect(type, "treatment") == TRUE, "case", "control")
   ) %>%
   drop_na(id)

 #Creates new dataset arranged by id number
 cc_matches_id <-
   cc_data_typhoid_vax_any %>%                    #Change this per dataset making
   mutate(row_num = as.character(row_number())) %>%
   #select(row_num) %>%
   left_join(matches, by = c("row_num" = "id")) %>%
   drop_na(pair) %>%
   arrange(pair, desc(styphi))

 ## Remove instances where there is no match to only retain matched cases and controls
 cc_matches_typhoid_vax_any <-  #change
   cc_matches_id %>%
   group_by(pair) %>%
   filter(n() != 1) %>%
   ungroup()

 plot(caco_results, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched


 cc_matches_typhoid_vax_any %>% #Print number matched cases and controls
   count(case)




 ################################## The following are checks to ensure that data looks ok ###################

 #Checking that the matching worked
 cc_matches %>%
   group_by(pair) %>% filter(n() > 1) %>%
   mutate(
     Dif_date = dt_enrolled - lag(dt_enrolled),
     Dif_age = age_mo_shot - lag(age_mo_shot)
   ) %>%
   group_by(pair) %>%
   ggplot(aes(Dif_age, Dif_date, color = pair)) +
   geom_jitter() +
   labs(y = "Time between enrollment within group (days)", x = "Difference in age within group (months)")

 #Counts number of discordant pairs by treatment location
 cc_matches %>%
   dplyr::select(pair, type) %>%
   pivot_wider(names_from = type, values_from = c(phase, TCV_vax)) %>%
   mutate(
     concordant = if_else(phase_treatment == phase_control.1 & phase_treatment ==  phase_control.2 & phase_treatment == phase_control.3, 1, 0)
   ) %>%
   count(phase_treatment, concordant)

 #Plots histogram of participant age (of matched data) by
 cc_matches %>%
   ggplot(aes(age_mo_shot/12, fill = styphi)) +
   geom_histogram(position = "dodge") +
   theme_bw() +
   labs(
     title = "Age distribution of case control matches",
     x = "Age of participant (year)",
     fill  = "Blood Culture Status",
     y = "Number of Individuals"
   )

 ## Plot cases and controls by month of enrollment
 cc_matches %>%
   ggplot(aes(cc_completed_time, fill = styphi)) +
   geom_histogram(position = "dodge") +
   theme_bw() +
   labs(
     title = "Age distribution of case control matches",
     x = "Age of participant (year)",
     fill  = "Blood Culture Status",
     y = "Number of Individuals"
   )


 library(tableone)
 cc_matches$chedu
 vars_list <- c("age_mo_shot","sex", "chedu" )
 ## Copy the CDC table of things to put in a table 1
 # hh size, education, etc
 factor_list <- c("sex", "chedu")

 CreateTableOne(data = cc_matches, vars = vars_list, factorVars = factor_list, strata = "cc_type")

 #Summarizations of data

 #Age
 cc_matches %>%
   filter(styphi == "Pos") %>%
   summarize(
     mean(age_mo_shot, na.rm = TRUE),
     median(age_mo_shot, na.rm = TRUE)
   )


 #Ages by bc status
 cc_data %>%
   group_by(site, styphi) %>%
   summarise(mean_age = mean(age_mo_shot, na.rm = TRUE), sd_age = sd(age_mo_shot, na.rm = TRUE)) %>%
   pivot_wider(names_from = styphi, values_from = c(mean_age, sd_age))

 #Boxplot of data
 cc_data %>%
   ggplot() +
   geom_boxplot(aes(site, AGEYR))

 #Vaccinated count from phase 1 areas of all cc_data
cc_data$tcv_vax

cc_matches$tcv_vax

 cc_data %>%
   count(TCV_vax_conserved)

 cc_data %>%
   filter(phase == 2) %>%
   count(TCV_vax)

 cc_matches %>%
   group_by(case) %>%
   count(tcv_vax)







