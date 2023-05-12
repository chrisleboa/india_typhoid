# Case control analysis Update

# Author: Chris LeBoa
# Version: 2023-03-17

# Libraries
install.packages("MatchIt")
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

## I think that we should use the MatchIT package like in the other analysis

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


#The age also must be within 1 year of the control

cc_filter <-
case_control_data %>%
  mutate(
    case = if_else(case_case_id!="",1,0),
    dt_enrolled = if_else(!is.na(cc_completed_time), cc_completed_time, today),
    dt_enrolled = as_date(dt_enrolled),
    months_from_shot = interval(as_date("2018-07-01"), dt_enrolled) / months(1),
    age_mo_shot = agemocalc - months_from_shot
    ) %>%
  drop_na(agemocalc)

#I need to separate cases and controls here as controls cannot have fever within 1 month whole

cc_data <-
  cc_filter %>%
  filter(
    dt_enrolled >= as_date("2018-09-01"),
    age_mo_shot >= 9 & age_mo_shot <= (14*12),
    fev1mo == 0 | is.na(fev1mo)
    )

cc_data %>%
  filter(case==1) %>%
  count(fev1mo)
#Confirming that all non cases are not missing a fever variable


# 4:1 NN PS matching w/o replacement
 caco_results <-
  matchit(case ~ agemocalc + dt_enrolled , data = cc_data, caliper = c(agemocalc = 12, dt_enrolled = 14),
          std.caliper = c(FALSE, FALSE),
        #  method = "exact",
          distance = "glm",
          ratio = 4)


 plot(caco_results, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched
 # In the data that was matched 5 cases are missing matches

 m.data2 <- match.data(caco_results, data = NULL,
                       distance = "prop.score")

 dim(m.data2) #one row per matched unit
 head(m.data2, 10)

 #Reorder matched data data frame with the groups in a single column
 matches <-
   data.frame(treatment= row.names(caco_results$match.matrix), control=caco_results$match.matrix) %>%
   mutate(pair = row_number()) %>%
   pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
   mutate(
     cc_type = if_else(str_detect(type, "treatment") == TRUE, "case", "control")
   ) %>%
   drop_na(id)

 #Creates new dataset arranged by id number
 cc_matches <-
   cc_data %>%
   mutate(row_num = as.character(row_number())) %>%
   #select(row_num) %>%
   left_join(matches, by = c("row_num" = "id")) %>%
   drop_na(pair) %>%
   arrange(pair, desc(styphi))

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
   dplyr::select(pair, type, phase, TCV_vax) %>%
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

 ############################## Vaccine Effectiveness Analysis #################

 ## Running of C-Logit Function to calculate the impact of vaccination on being a case

 summary(clogit(case ~ factor(tcv_vax) + strata(pair), cc_matches))
 ## Do histogram enrollment of cases and controls

 ## Calculating Vaccine Effectiveness
 # 1 - OR  =  VE
 # Form this reference https://apps.who.int/iris/bitstream/handle/10665/264550/PMC2491112.pdf
 # Since the OR was calculated in the clogit as the beta for
 # having a typhoid vaccination exp(coef)

 #For Beta
 exp(-1.7579) #0.172
 1 - 0.172  ## Overall VE is 82.8%

 #Use the exponentiated Lower and upper bounds to get the 95% CI
   #These are listed in the summary of the clogit
 1- 0.04977  # 0.95
 1-0.597.    # 0.403







