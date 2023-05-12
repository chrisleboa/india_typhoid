## 3_Figure_numbers

#Figure 1 (flowchart numbers)

all_data %>%
  mutate(case = if_else(styphi == "Pos", 1, 0)) %>%
  count(ENRL_TYPE)

all_data %>%
  filter(phase == 1) %>%
  mutate(case = if_else(styphi == "Pos", 1, 0)) %>%
  count(ENRL_TYPE, enrstyphi)

### Use the cc filter from the case control dataset to get file with the age mo since vaccination variable

cc_filter %>%
  filter(case == 0) %>%
  filter(
    dt_enrolled >= as_date("2018-09-01"),  #Date enrolled is after campaign
    age_mo_shot >= 9 & age_mo_shot <= (14*12), #Age is between 9 months and 14 at time of campaign
    fev1mo == 0 | is.na(fev1mo),         #there is no fever in last month
    tcv_vax == 99
    )

cc_filter %>%
  filter(case == 1) %>%
  filter(
    dt_enrolled >= as_date("2018-09-01"),  #Date enrolled is after campaign
    age_mo_shot <= 9 & age_mo_shot <= (14*12), #Age is between 9 months and 14 at time of campaign
    fev1mo == 0 | is.na(fev1mo)          #there is no fever in last month
  )
