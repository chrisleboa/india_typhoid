
############################## Vaccine Effectiveness Analysis #################

#There are a series of 6 analyses to be performed to get the overall vaccine effectiveness,the vaccine effectiveness broken down by vaccination type, and by age
#Age based analysis are done using the tcv_any category of vaccinatioon
#variables are generated in the prior script 1_matching_cases_and_controls.R

## Calculating Vaccine Effectiveness
# 1 - OR  =  VE
# Form this reference https://apps.who.int/iris/bitstream/handle/10665/264550/PMC2491112.pdf
# Running of C-Logit Function to calculate the impact of vaccination on being a case
# OR was calculated in the clogit as the exp(beta) for being vaccinated
# The lower and upper confindence intervals are taken from the summary of each model as well


#1 TCV Campaign card only
summary(clogit(case ~ factor(typhoid_campaign_card) + strata(pair), cc_matches_tcv_card))

cc_matches_tcv_card_recall %>% count(case, typhoid_campaign_card_recall)

#2 TCV Campaign cardor recall
summary(clogit(case ~ factor(typhoid_campaign_card_recall) + strata(pair), cc_matches_tcv_card_recall))

#Thoughts on results for analysis 2
#For some reason the results for these two analyses are different probably because of the way that the controls are selected
#There is a looser definition of who is vaccinated in the control population
 #-- which boosts numbers of vaccinated that are not cases
 # more vaccinated in the control population == more ppl with vaccines that didnt get typhoid in the analysis == vaccine seems more effective

#3 Using TCV vax variable (other variable in orig dataset)
#### THIS IS THE OVERALL ANALYSIS USED FOR PAPER OVERALL Result#####

summary(clogit(case ~ factor(tcv_vax) + strata(pair), cc_matches_tcv_vax))

cc_matches_tcv_vax %>% count(case, tcv_vax)



#4 Using TCV any variable (Less sure how this one is defined in terms of what the #3's are)
summary(clogit(case ~ factor(tcv_any) + strata(pair), cc_matches_tcv_any))

#5 Using any Typhoid vaccine variable
summary(clogit(case ~ factor(typhoid_vax_any) + strata(pair), cc_matches_typhoid_vax_any))
    #See results.csv for the results from each of these analyses
cc_matches_typhoid_vax_any %>% count(case, typhoid_vax_any)

## Split vaccine effectiveness by age

#6 Over 5 years at time of shot
summary(clogit(case ~ factor(tcv_vax) + strata(pair), cc_matches_tcv_vax %>% filter(age_mo_shot >= 60)))



cc_matches_tcv_vax %>% filter(age_mo_shot >= 60) %>% count(case, tcv_vax)
  #27 cases and 107 controls
1-0.129 #0.871
 #lowr
1 - 0.029 #0.97
1 - 0.571 #upper #0.43

#2- 5 years at time of shot
summary(clogit(case ~ factor(tcv_vax) + strata(pair), cc_matches_tcv_vax %>% filter(age_mo_shot >= 24 & age_mo_shot <= 60)))

cc_matches_tcv_vax %>% filter(age_mo_shot <= 60) %>% count(case, tcv_vax)
    #9 cases and 33 controls

1 -0.276 #0.724
1 - 0.031 #0.969
1 -2.44 #-1.44

#<2 years at time of shot
summary(clogit(case ~ factor(tcv_vax) + strata(pair), cc_matches_tcv_vax %>% filter(age_mo_shot <= 24)))

cc_matches_tcv_vax %>% filter(age_mo_shot <= 24) %>% count(case)

cc_matches_tcv_vax %>%
  filter(age_mo_shot <24)


