# Case control matching

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


data_location <- "/Users/ChrisLeBoa/Box Sync/CDC_Typhoid_data/tcv_all__2021_04_18.sas7bdat"
#dataset with all data in it up to March 2021

#===============================================================================

#Read in data
all_data <- read_sas(data_location)

all_data$TCV_vax

#Remove non-eligible cases / controls
cc_data <-
  all_data %>%
  filter(
    !is.na(styphi),                                 #blood culture pos for typhi
    cc_completed_time >= as_date("2018-09-01"),      #select participants after september 1 2018
    (AGEYR <= 15 & AGEYR >= 0) | (AGEYR == 0 & AGEMO >= 9),      # Max age less than 16     # Min age > 9 months
    LIVE_NMMC == 1,                                  # Select children that live in NMMC
    str_detect(UHP, "NO_") == FALSE  # Remove UHP's marked as No for not in NMMC
  ) %>%
  mutate(TR = if_else(styphi == "Pos", 1, 0)) %>%
  replace_na(list(ve_nctcv = 0, ve_tcvcard = 0, ve_ncothtyp_a = 0))

# 3:1 NN PS matching w/o replacement
m.out2 <-
  matchit(TR ~ AGEYR + cc_completed_time, data = cc_data, caliper = c(AGEYR = 1, cc_completed_time = 28),
          std.caliper = c(FALSE, FALSE),
#method = "exact",
distance = "glm",
ratio = 3)


m.out2$match.matrix

plot(m.out2, type = "jitter", interactive = FALSE)

m.data2 <- match.data(m.out2, data = NULL,
                      distance = "prop.score")

#m.data2 <- get_matches(m.out2,id = "id",
#                      distance = "prop.score")
dim(m.data2) #one row per matched unit
head(m.data2, 10)


matches <-
  data.frame(treatment= row.names(m.out2$match.matrix), control=m.out2$match.matrix) %>%
  mutate(pair = row_number()) %>%
  pivot_longer(cols = c("treatment", starts_with("c")), names_to = "type", values_to = "id") %>%
  drop_na(id)

cc_matches <-
  cc_data %>%
  mutate(row_num = as.character(row_number())) %>%
  #select(row_num) %>%
  left_join(matches, by = c("row_num" = "id")) %>%
  drop_na(pair) %>%
  arrange(pair, desc(styphi))

cc_matches %>%
  group_by(pair) %>% filter(n() > 1) %>%
  mutate(
    Dif_date = cc_completed_time - lag(cc_completed_time),
    Dif_age = AGEYR - lag(AGEYR)
  ) %>%
  group_by(pair) %>%
  ggplot(aes(Dif_age, Dif_date, color = pair)) +
  geom_jitter()


cc_matches %>%
  as_tibble() %>%
  dplyr::select(pair, styphi, phase, TCV_vax) %>%
  ConDisPairs()

#Don't list concordant pairs like this for multiple controls
cc_matches %>%
  dplyr::select(pair, type, phase, TCV_vax) %>%
  pivot_wider(names_from = type, values_from = c(phase, TCV_vax)) %>%
  mutate(
    concordant = if_else(phase_treatment == phase_control.1 & phase_control.2 & phase_control.3, 1, 0)
    ) %>%
  count(phase_treatment, concordant)

cc_matches %>%
  ggplot(aes(AGEYR, fill = styphi)) +
  geom_histogram(position = "dodge") +
  labs(
    title = "Age distribution of case control matches",
    x = "Age of participant (year)",
    fill  = "Blood Culture Status",
    y = "Number of Individuals"
    )

cc_matches %>%
  filter(styphi == "Pos") %>%
  summarize(
    mean(AGEYR, na.rm = TRUE),
    median(AGEYR, na.rm = TRUE)
    )
