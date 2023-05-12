# Unmatched addresses India

# Author: Name
# Version: 2022-05-04

# Libraries
library(tidyverse)

# Parameters
matched_data <- read_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Extract_Data_May_4__2022__1_47_19_PM.csv")
all_data <- read_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Addressdata_220422_Vikas.csv")
not_matched_data <- "/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Addressdata_to_match.csv"

missing_data_found <- read_csv("/Users/ChrisLeBoa/Documents/Work/Research/Luby Lab/India/GIS Data/Positive Case Identification /Missing_GPS_pt_0.csv")
matched_data_confirmed <- read_csv("/Users/ChrisLeBoa/Documents/Work/Research/Luby Lab/India/GIS Data/Positive Case Identification /Addressdata_220523_Checked/Addressdata_220422_Vikas_0.csv")
output_dataset <- "/Users/ChrisLeBoa/Documents/Work/Research/Luby Lab/India/GIS Data/Positive Case Identification /address_data_checked_220523"

#===============================================================================
#I ran the data through ARCGIS GEOPOINT FEATURE IT matched 125 locations and was missing 50 Locations

#This section outputs seprate csv's for those that it matched and those that were missing
#matched locations were reuploaded to online map
#missing locations were created as a seprate empty geodatabase and enumerator sent to go find location from the address
study_id_match <- matched_data %>% select(Studyid_TCV) %>% pull()


all_data %>%
  filter(!(Studyid_TCV %in% study_id_match)) %>%
  write_csv(not_matched_data)


#### NOW THAT i downloaded the missing data points again

#Once data were corrected for gps points I update the all data file with the new GPS points

miss_minus_id <-
  missing_data_found %>%
  select(-OBJECTID)

all_confirmed <-
  matched_data_confirmed %>%
  select(Studyid_TCV, phase, x, y) %>%
  bind_rows(miss_minus_id)

all_data %>%
  left_join(all_confirmed, by = "Studyid_TCV") %>%
  mutate(
    phase_manual = phase.y,
    phase = phase.x
         ) %>%
  write_csv(output_dataset)






