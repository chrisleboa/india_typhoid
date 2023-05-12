# Meeting with seth to go over R

# Author: Chris LeBoa
# Version: 2023-01-05


# Libraries
#install.packages("haven")

library(haven) #Reads in sas files
library(tidyverse)

# Parameters

data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18.sas7bdat"

data_location_2 <- here::here("Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18.sas7bdat")
#dataset with all data in it up to March 2021

#===============================================================================

#Code

all_data <- read_sas(data_location)


all_data %>%
  filter(formtype == "CLN") %>%
  count(AGEYR)

all_data %>% count(formtype)

