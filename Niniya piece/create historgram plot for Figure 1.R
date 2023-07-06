# create historgram plot for Figure 1

# Libraries
library(tidyverse)
library(haven)
library(fs)
library(tableone)
library(lubridate)
library(MatchIt)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(lattice)
library("ggplotify")
library(devtools)
library(easyGgplot2)

#histogram of male v female


#full AMR data set
amrdata<-read_excel("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/Niniya piece/Master_data file_8.7.2022_SAHupdated_7.15.22.xlsx")
amrdata_noAge<-read_excel("/Users/ChrisLeBoa/GitHub/typhoid_research/india_typhoid/Niniya piece/Master_data file_8.7.2022_SAHupdated_7.15.22_noNAs.xlsx")

amrdata$Date_US<-format(as.Date(amrdata$Date_US),'%m/%d/%Y')
amrdata_noAge$Date_US<-format(as.Date(amrdata_noAge$Date_US),'%m/%d/%Y')

#stacked histogram

# install.packages("devtools")
#library(devtools)
#install_github("kassambara/easyGgplot2")
library(ggplot2)
# i want to display frequency, age, and gender
histogram<-
  amrdata_noAge %>%
  mutate(age_group = case_when(
    Joshi_Age <2 ~ "Under 2",
    Joshi_Age >= 2 & Joshi_Age < 5 ~ "2 - 5 ",
    Joshi_Age >= 5 & Joshi_Age < 10 ~ "5 - 10",
    Joshi_Age >= 10 & Joshi_Age < 15 ~ "10 - 15 ",
    Joshi_Age >= 10 & Joshi_Age < 15 ~ "> 15"
  ),
  age_group = as.factor(age_group)) %>%
  ggplot() +
  geom_jitter(aes(
    x = Joshi_Age,
    y = as.factor(Gender),
    color = Gender
  ), width = 0.25)+
  geom_boxplot(
    aes(
      x = Joshi_Age,
      y = as.factor(Gender),
      color = Gender
    ),
    alpha = 0.8,
    width = 0.2) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0, 70, 5)) +
  scale_color_grey( start = 0.2,
                    end = 0.8)+
  theme(legend.position = "none")  +
  labs(
    title = "Age and Gender of Enteric Fever Cases: 2015-2018",
    x = "Age",
    y = ""
  )

