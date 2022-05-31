# CC Matches Additions

# Author: Chris leBoa
# Version: 2021-10-20

# Libraries

#input -- cc_matches_case -- from the cc Matches file


cc_matches_case_adj <-
  cc_matches_case %>%
  mutate(
    age_bin = if_else(AGEYR > 7, 1, 0), #If child is over 7 years old gets 1 otherwise 0
    edu_bin = if_else(edu > 2, 1, 0) # if respondent has more than grade 4 education 1 otherwise 0
  )

clogit_adjusted <-
  clogit(case ~ factor(vaccinated_area) + factor(edu_bin) + factor(age_bin) + factor(sex) + strata(pair), cc_matches_case_adj)

clogit_unadjusted <-
  clogit(case ~ factor(vaccinated_area), cc_matches_case_adj)


coef(clogit_adjusted)




