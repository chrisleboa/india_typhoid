# Case control matching
      ## This script matches S.Typhi cases with up to three controls and runs
      ## the data through the clogit fxn from the survival package

   ## ToDo:  Run analysis from time before campaign and see if still see a result
   ## Run Analysis with other disease and see if there is a similar result to Typhi


# Author: Chris LeBoa
# Version: 2021-06-07

#install.packages("Matching")

# Libraries
library(tidyverse)
library(haven)
library(fs)
library(tableone)
library(lubridate)
library(MatchIt)
library(survival)


data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18_vax_08_31.sas7bdat"
#dataset with all data in it up to March 2021

#===============================================================================

#Read in data
all_data <- read_sas(data_location)


view(all_data)

#Remove non-eligible cases / controls

cc_data <-
  all_data %>%
  filter(
  #  !is.na(styphi),                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
  ) %>%
  mutate(TR = if_else(styphi == "Pos", 1, 0)) %>%
  replace_na(list(ve_nctcv = 0, ve_tcvcard = 0, ve_ncothtyp_a = 0))

# 3:1 NN PS matching w/o replacement
m.out2 <-
  matchit(TR ~ AGEYR + cc_completed_time + site , data = cc_data, caliper = c(AGEYR = 1, cc_completed_time = 28),
          std.caliper = c(FALSE, FALSE),
#method = "exact",
distance = "glm",
ratio = 3)

### Right now we have site included but we may remove this ultimately

m.out2$match.matrix

plot(m.out2, type = "jitter", interactive = FALSE) #This plots the number matched and the number unmatched

m.data2 <- match.data(m.out2, data = NULL,
                      distance = "prop.score")

dim(m.data2) #one row per matched unit
head(m.data2, 10)

#Reorder matched data data frame with the groups in a single column
matches <-
  data.frame(treatment= row.names(m.out2$match.matrix), control=m.out2$match.matrix) %>%
  mutate(pair = row_number()) %>%
  pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
  drop_na(id)

#Creates new data set of only matched data arranged by pair number
cc_matches <-
  cc_data %>%
  mutate(row_num = as.character(row_number())) %>%
  #select(row_num) %>%
  left_join(matches, by = c("row_num" = "id")) %>%
  drop_na(pair) %>%
  arrange(pair, desc(styphi))

## Plots the calendar month and age differences of the matches
cc_matches %>%
  group_by(pair) %>% filter(n() > 1) %>%
  mutate(
    Dif_date = cc_completed_time - lag(cc_completed_time),
    Dif_age = AGEYR - lag(AGEYR)
  ) %>%
  group_by(pair) %>%
  ggplot(aes(Dif_age, Dif_date, color = pair)) +
  geom_jitter()


#Counts number of discordant pairs by treatment location
cc_matches %>%
  dplyr::select(pair, type, phase, TCV_vax) %>%
  pivot_wider(names_from = type, values_from = c(phase, TCV_vax)) %>%
  mutate(
    concordant = if_else(phase_treatment == phase_control.1 & phase_treatment ==  phase_control.2 & phase_treatment == phase_control.3, 1, 0)
    ) %>%
  count(phase_treatment, concordant)

#Plots histogram of participant age (of matched data) by
cc_matches %>%
  ggplot(aes(AGEYR, fill = styphi)) +
  geom_histogram(position = "dodge") +
  labs(
    title = "Age distribution of case control matches",
    x = "Age of participant (year)",
    fill  = "Blood Culture Status",
    y = "Number of Individuals"
    )


#Summarizations of data

#Age
cc_matches %>%
  filter(styphi == "Pos") %>%
  summarize(
    mean(AGEYR, na.rm = TRUE),
    median(AGEYR, na.rm = TRUE)
    )

#Site and phase
cc_data %>%
  count(site, phase) %>%
  pivot_wider(names_from = phase, values_from = n)

#Ages by bc status
cc_data %>%
  group_by(site, styphi) %>%
  summarise(mean_age = mean(AGEYR, na.rm = TRUE), sd_age = sd(AGEYR, na.rm = TRUE)) %>%
  pivot_wider(names_from = styphi, values_from = c(mean_age, sd_age))

#Boxplot of data
cc_data %>%
  ggplot() +
  geom_boxplot(aes(site, AGEYR))

#Vaccinated count from phase 1 areas of all cc_data
cc_data %>%
  filter(phase == 1) %>%
  count(TCV_vax_conserved)

cc_data %>%
  filter(phase == 2) %>%
  count(TCV_vax)


cc_data %>%
  filter(styphi == "Pos") %>%
  count(TCV_vax_conserved, TCV_vax, ve_tcvcard)

cc_data %>%
  count(phase, vacrecall, dose)

cc_data %>%
  filter(card == 1 & cardobs == 1) %>%
  count(cardtyphoid, vacrecall, dose)


## Running of C-Logit Function


cc_matches_case <-
  cc_matches %>%
  mutate(
    case = if_else(str_detect(type, "treat"), 1, 0))

summary(clogit(case ~ factor(phase) + strata(pair), cc_matches_case))

