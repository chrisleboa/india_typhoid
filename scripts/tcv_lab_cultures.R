# Navi Mumbai Lab Useage 2019 - 2020

# Author: Chris LeBoa
# Version: 2021-01-28

# Libraries
library(tidyverse)

# File Location

file_in <- "/Users/ChrisLeBoa/Documents/Work/Research/Luby Lab/India/Data Analysis /lab_data/navimumbai_lab_cultures.xlsx"

joshi_data <- readxl::read_excel(file_in)

#===============================================================================

#Code

joshi_data %>%
  group_by(Year) %>%
  summarise(
    sum(tcv_bc_run),
    sum(total_bc_nmmc),
    sum(bc_u17_nmmc)
    )

joshi_data %>%
  mutate(Year = as_factor(Year)) %>%
  group_by(Year) %>%
  ggplot(aes(Month, tcv_bc_run, color = Year)) +
  geom_point() +
  geom_line(aes(group = Year)) +
  labs(
    title = "Comparison of Typhi Blood Cultures Run between 2019 and 2020 by Joshi Lab",
    y = "Monthly TCV Blood Cultures Run"
  )
