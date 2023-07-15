# Flow chart Vaccine effect paper numbers

#The flow chart we are using is located on Lucid linked here
#https://lucid.app/lucidchart/dd75a082-eb1c-4fd6-805d-1b1ee886f7e8/edit?shared=true&page=0_0&invitationId=inv_a366d877-9d65-41ec-919a-0facb041745e#
#To run this file is good to first run 1_matching_cases_and_controls.R which creates the cc_filter dataset

# Author: Chris LeBoa
# Version: 2023-07-15

# Libraries
library(tidyverse)
library(haven)

# Parameters

#Number
#number of children in the control pool (10878)
ccs_data <- read_sas("/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/ccs_ch_2020_04_24_ver2.sas7bdat")

#Number of clinical enrollments (4304)
clin_enroll_data <- read_sas("/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/clin_enrl__2021_04_18_2021_08_31.sas7bdat")
#number of lab enrollments (298)
lab_enroll_data <- read_sas("/Users/ChrisLeBoa/Library/CloudStorage/Box-Box/CDC_Typhoid_data/lab_enrl__2021_04_18.sas7bdat")
#===============================================================================

### Calculating numbers for figure 1

cc_filter %>%
  count(case, tcv_vax)

cc_filter %>%
  filter(
    fev1mo != 1,
    age_mo_shot >= 9 & age_mo_shot <= (14*12)) %>%
  count(case)

cc_data %>%
  count(tcv_vax, case)

