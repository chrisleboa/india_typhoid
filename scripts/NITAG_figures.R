# This compiles the breakdown of ages and times of enrollment in the study
#Jason wanted it in a "Wes Anderson" color scheme

# Author: Chris LeBoa
# Version: 2021-09-27

#devtools::install_github("karthik/wesanderson")

# Libraries
library(tidyverse)
library(wesanderson)
library(gridExtra)
library(ggsci)

# Parameters

#===============================================================================

#Code

#Plots histogram of participant age (of matched data) by
plot_1 <-
  cc_matches_case %>%
  mutate(cases = if_else(case != 1, "Blood-culture negative cases”", "Blood-culture positive cases")) %>%
  ggplot(aes(AGEYR, fill = cases)) +
  geom_histogram(position = "dodge") +
  theme_bw() +
  theme(legend.position = c(.8,.9), legend.key.size = unit(1, 'cm')) +
  expand(c(0,0))
  scale_fill_nejm() +
  labs(
    title = "Age distribution of case control matches",
    x = "Age of participant in years",
    fill  = "",
    y = "Number of Individuals"
  )

by_month <- function(x,n=1){
  seq(min(x,na.rm=T),max(x,na.rm=T),by=paste0(n," months"))
}



## Plot cases and controls by month of enrollment
plot_2 <-
  cc_matches_case %>%
  mutate(cases = if_else(case != 1, "Blood-culture negative cases”", "Blood-culture positive cases")) %>%
  #filter(cases == "Case") %>%
  ggplot(aes(cc_completed_time, fill = cases)) +
  geom_histogram(breaks = by_month(cc_matches_case$cc_completed_time), position = "dodge") +
  geom_vline(xintercept = as.numeric(as.Date("2018-08-30")), linetype=4) +
  scale_x_date(labels = scales::date_format("%Y-%b"),
               breaks = by_month(cc_matches_case$cc_completed_time,6)) +
  annotate(geom = "text",
           x = as.Date("2018-09-20"),
           y = 16,
           label = "End TCV Vaccine Campaign",
           color = "black",
           angle = 90) +
  theme(axis.text.x = element_text(angle=90)) +
  theme_bw() +
  theme(legend.position = c(.80,.9), legend.key.size = unit(1, 'cm')) +
  #scale_fill_manual(values = wes_palette("Cavalcanti1")) +
  scale_fill_nejm() +
  labs(
    title = "Case-Control Enrollment Distribution",
    x = "Month of enrollment",
    fill  = "",
    y = "Number of Individuals"
  )
##Get rid of negatives

cc_matches_case %>%
  filter(case != 1) %>%
  summarize(mean(AGEYR, na.rm = TRUE))

cc_matches_case %>%
  filter(case == 1, AGEYR <= 5) %>%
  summarize(mean(AGEYR, na.rm = TRUE))

plot_1
plot_2
