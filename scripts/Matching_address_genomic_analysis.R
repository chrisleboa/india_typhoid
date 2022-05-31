# Unmatched addresses India

# Author: Name
# Version: 2022-05-04

# Libraries
library(tidyverse)

# Parameters
matched_data <- read_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Extract_Data_May_4__2022__1_47_19_PM.csv")
all_data <- read_csv("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Addressdata_220422_Vikas.csv")
not_matched_data <- "/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Addressdata_to_match.csv"
#===============================================================================

study_id_match <- matched_data %>% select(Studyid_TCV) %>% pull()


all_data %>%
  filter(!(Studyid_TCV %in% study_id_match)) %>%
  write_csv(not_matched_data)
