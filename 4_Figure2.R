# Figure 2 code
  #This file creates a forest plot of the model results from the VE analysis of the india typhoid vaccine data
# Author: Chris LeBoa
# Version: 2023-05-03

# Libraries
library(tidyverse)
library(readxl)


# Parameters
data_location <- "/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/Vaccine Case Control Analysis/TCV_VE_analysis_model_results.xlsx"

data <- read_xlsx(data_location)
#===============================================================================

# Code

ggplot(data=data, aes(y= Model, x=VE, xmin=upper_VE, xmax=low_VE)) +
  geom_point() +
  geom_errorbarh(height=.1) +
  #scale_y_discrete(name = "", breaks=1:nrow(data), labels=data$Model) +
  scale_x_continuous(label= scales::label_percent()) +
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  theme_minimal()





