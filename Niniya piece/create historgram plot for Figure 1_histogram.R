# create historgram plot for Figure 1

# Libraries
library(tidyverse)
library(readxl)

#histogram of male v female


#full AMR data set
amrdata<-read_excel("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/Niniya piece/Master_data file_8.7.2022_SAHupdated_7.15.22.xlsx")
amrdata_noAge<-read_excel("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/Niniya piece/Master_data file_8.7.2022_SAHupdated_7.15.22_noNAs.xlsx")

amrdata$Date_US<-format(as.Date(amrdata$Date_US),'%m/%d/%Y')
amrdata_noAge$Date_US<-format(as.Date(amrdata_noAge$Date_US),'%m/%d/%Y')

#stacked histogram


# i want to display frequency, age, and gender
histogram<-
  amrdata_noAge %>%
  mutate(age_group = case_when(
    Joshi_Age < 2 ~ "<2",
    Joshi_Age >= 2 & Joshi_Age < 5 ~ "2-<5",
    Joshi_Age >= 5 & Joshi_Age < 15 ~ "5-<15",
    Joshi_Age >= 15 & Joshi_Age <= 45 ~ "15-<45",
    Joshi_Age > 45  ~ "45 +"
  ),
  age_group = factor(age_group, levels = c("<2","2-<5","5-<15","15-<45","45 +"))) %>%
  #age_group = factor(age_group, levels = c("0-3","4-6","7-9","10-14","15-24","25-34","35-44","45 +"))) %>%
 # group_by(age_group) %>%
 # summarise(number = n()) %>%
  drop_na(age_group) %>%
  ggplot() +
  geom_bar(aes(x = age_group, fill = factor(Gender)), position = "dodge") +
  theme_bw() +
  scale_fill_grey(start = 0.6,
                  end = 0.9) +
  #scale_color_brewer(palette = "ggplot2")+
  #theme(legend.position = "top")  +
  labs(
    title = "Age and Gender of Enteric Fever Cases: 2015-2018",
    x = "Age Group",
    y = "Number of Cases",
    fill = "Gender"
  )

