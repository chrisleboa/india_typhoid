# Sending Kesia Merged Dataset of Navi Mumbai Data for genomic analysis
    #This script pulls the lab codes and merges them with the study ids from the main dataset
    # It retains only the samples that have a laboratory ID

# Author: Chris leBoa
# Version: 2021-09-03

# Libraries
library(tidyverse)
library(haven)

# Parameters

all_data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18_vax_08_31.sas7bdat"
lab_data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/MIC_23032021.xlsx"
combined_data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/combined_genomic_data.csv"

all_data <- read_sas(all_data_location)
lab_data <- readxl::read_excel(lab_data_location)

#===============================================================================

# Merge Datasets

#Check names of columns to join by
datasetall_data$studyid
lab_data$studyid

#Join Datasets
lab_data %>%
  left_join(all_data, by = "studyid") %>%
  write_csv(combined_data_location)

lab_data %>%
  left_join(all_data, by = "studyid") %>%
  filter(!is.na(studyid), !is.na(ve_completed_time)) %>%
  select(ve_gps_latitude_degrees_) %>%
  view()



