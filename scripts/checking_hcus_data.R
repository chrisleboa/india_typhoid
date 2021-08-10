# Checking HCUS
# Author: Chris LeBoa
# Version: 2021-05-05

# Libraries
#install.packages("haven")
library(tidyverse)
library(haven)
library(fs)
library(tableone)
library(lubridate)

#Updated analysis dataset
data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18.sas7bdat"

# Code
all_data <- read_sas(data_location)

## Breakdown of vaccination status in cases
all_data %>%
  filter(
    styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
  ) %>%
  replace_na(list(ve_nctcv = 0, ve_tcvcard = 0, ve_ncothtyp_a = 0)) %>%
  count(phase, TCV_vax) %>%
  knitr::kable()

#Graph of typhi data and vaccine breakthrough types
all_data %>%
  filter(
    styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
  ) %>%
  replace_na(list(ve_nctcv = 0, ve_tcvcard = 0)) %>%
  mutate(
    tcv_vax = if_else(TCV_vax != "No", "Vaccinated", "Unvaccinated")) %>%
  ggplot(aes(cc_completed_time, AGEYR, color = as_factor(TCV_vax), shape = as_factor(phase), size = TCV_vax)) +
  geom_point(size = 4) +
  theme(legend.position = "bottom") +
  labs(
    title = "Age and vaccination status of S.Typhi cases",
    x = "Date of enrollment",
    y = "Case Age (yrs)",
    shape = "Vaccination Campaign Phase",
    color = "Vaccination Status"
  )

## Healthcare utilization combined
View(all_data)

all_data %>%
  filter(
    #ENRL_TYPE == 1,                                 #Only clinical enrollment
    #styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
    ) %>%
  count(ENRL_TYPE, phase, results_yn) %>%
  knitr::kable()

all_data %>%
  filter(
    #ENRL_TYPE == 1,                                 #Only clinical enrollment
    #styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE,  # Remove UHP's marked as No for not in NMMC
    is.na(phase)
  ) %>%
  #select(UHP, phase) %>%
  count(UHP, phase, results_yn, styphi)





all_data %>%
  filter(
    #ENRL_TYPE == 1,                                 #Only clinical enrollment
    #styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE,  # Remove UHP's marked as No for not in NMMC
    is.na(phase)
  ) %>%
  ggplot(aes(FEVERDAYS)) +
  geom_histogram(fill = "darkred") +
  labs(
    title = "Distribution of time of fever before enrollment",
    y = "Number of patients",
    x = "Days of Fever symptopms"
  )



all_data %>%
  filter(
    #ENRL_TYPE == 1,                                 #Only clinical enrollment
    #styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE,  # Remove UHP's marked as No for not in NMMC
    is.na(phase)
  ) %>%
  ggplot(aes(oschdaysmiss)) +
  geom_histogram(fill = "darkred", binwidth = 1, center = .5) +
  labs(
    title = "Number of school days missed due to fever",
    y = "Number of patients",
    x = "Days"
  )


## Show distribution of ages in case data



