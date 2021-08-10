# Checking assumptions of case data conducted by CDC
# Author: Chris LeBoa
# Version: 2021-03-26

# Libraries
#install.packages("haven")
library(tidyverse)
library(haven)
library(fs)
library(tableone)
library(lubridate)

data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18.sas7bdat"
      #dataset with all data in it up to March 2021


# Code
all_data <- read_sas(data_location)

all_data$TCV_vax


## What proportion of cases received TCV vaccine? How does this compare over time and between phase 1 and phase 2 communities?

all_data %>%
  filter(
    styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
  ) %>%
  replace_na(list(ve_nctcv = 0, ve_tcvcard = 0, ve_ncothtyp_a = 0)) %>%
  count(phase, TCV_vax)




all_data %>%
  filter(
    styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
    #ENRL_TYPE == 2,
    # phase == 99
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



all_data %>%
  filter(
    styphi == "Pos",                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
  ) %>%
  replace_na(list(ve_nctcv = 0, ve_tcvcard = 0, ve_ncothtyp_a = 0)) %>%
  filter(phase == 2, TCV_vax == "Yes (VE)") %>%
  write_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/phase_2_ve.csv")
