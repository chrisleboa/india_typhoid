# This script creates a graph of the retrospective cases of Typhi from Joshi lab


# Author: Chris LeBoa
# Version: 2021-10-30

# Libraries
library(tidyverse)
library(lubridate)
library(readxl)

# Parameters

data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/data/Retrospective. seasonality datat.xlsx"
#===============================================================================

#Code

data <- read_xlsx(data_location, sheet = 2)  %>%
  pivot_longer(cols = -Year, names_to = "month", values_to = "cases") %>%
  mutate(month = as.numeric(month))# %>%
  #mutate(month = month(month, label = TRUE))



data %>%
  ggplot() +
  geom_boxplot(aes(month(month, label = TRUE), cases)) +
  geom_smooth(aes(month, cases), se = FALSE, color = "red") +
  labs(
    title = "Joshi Lab Diagnosed Typhi Cases 2014 - 2018",
    x = "Month",
    y = "Cases"
  )
