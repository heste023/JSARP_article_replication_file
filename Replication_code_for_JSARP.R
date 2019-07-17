library(tidyverse)
library(stargazer)
library(lme4)
library(rms)

##################
# first dataset #
##################

# load da35012.001 from data file

# filter to currently enrolled students 
da35012.0001 %>%
  select(DIDVOTE, REGISTERED, RACE_FINAL2, QGENDER, PARTYID, CONSERVATIVE, SCHOOLENROLL, 
         RCODE, SDR12, INPSNERLYVOTE, MAILVOTE, ABS_NEXC,  
         VOTEIDREQ, BATTLEGROUND,
         ONL12, XTND_HRS, LEGID, PHOTOIDREQ,  R_ERLYVT, PHOTOIDREQ,
         STATE, ELE12COMP, WEIGHT_FINAL, ONL12) %>%
  filter(SCHOOLENROLL == "(2) Currently enrolled") %>%
  {.} -> d2

# recode variables 
d2 %>%
  mutate(Voted = ifelse(DIDVOTE == '(1) Did vote in 2012', 1, 0)) %>%
  mutate(register = ifelse(REGISTERED == '(1) Registered', 1, 0)) %>%
  mutate(white = ifelse(RACE_FINAL2 == '(1) White non-Hispanic', 1, 0)) %>%
  mutate(strict = ifelse(PHOTOIDREQ == "(2) In effect for 2012 election", 1, 0)) %>%
  {.} -> d2.1

# remove missing values
d2.1 <- na.omit(d2.1)

d2.1 %>%
  mutate(White = ifelse(RACE_FINAL2 == '(1) White non-Hispanic', 1, 0)) %>%
  mutate(Black = ifelse(RACE_FINAL2 == '(2) Black non-Hispanic', 1, 0)) %>%
  mutate(Native_American = ifelse(RACE_FINAL2 == '(3) Native American non-Hispanic', 1, 0)) %>%
  mutate(Asian = ifelse(RACE_FINAL2 == '(4) Asian American-Pacific Islander non-Hispanic', 1, 0)) %>%
  mutate(Hispanic = ifelse(RACE_FINAL2 == '(6) Hispanic', 1, 0)) %>%
  mutate(Multiracial = ifelse(RACE_FINAL2 == '(7) Multiracial', 1, 0)) %>%
  mutate(Female = ifelse(QGENDER == '(2) Female', 1, 0)) %>%
  mutate(In_Person_Early_Voting = ifelse(INPSNERLYVOTE == '(1) In person early voting allowed', 1, 0)) %>%
  mutate(Mail_Voting = ifelse(MAILVOTE == '(1) Mail voting is available 2012', 1, 0)) %>%
  mutate(No_Excuse_Absentee_Voting = ifelse(ABS_NEXC == '(1) No-excuse absentee voting available', 1, 0)) %>%
  mutate(battle_state = ifelse(BATTLEGROUND == '(0) Not battleground', 0, 1)) %>%
  mutate(battle_state_2 = ifelse(ELE12COMP == "(2) Complete toss-up", 1, 0)) %>%
  mutate(Strict_Voter_ID_Required = ifelse(PHOTOIDREQ == '(2) In effect for 2012 election', 1, 0)) %>%       
  mutate(Online_Registration_Allowed = ifelse(ONL12 == 1, 1, 0)) %>%
  {.} -> d2.1

# tidy dataset 
d2.1 %>%
  select(Voted, Weight = WEIGHT_FINAL, Strict_Voter_ID_Required, White, Black, Native_American, Asian, Hispanic, Multiracial, Female, Same_Day_Registration = SDR12, 
         onlinereg = ONL12, In_Person_Early_Voting, Mail_Voting, No_Excuse_Absentee_Voting, battle_state, Campaign_Competitiveness = battle_state_2, 
         state = STATE, Online_Registration_Allowed) %>%
  {.} -> d2.1

# logistic regression - general election, minorites
m1 <- glm(Voted ~ 
            In_Person_Early_Voting + Mail_Voting + No_Excuse_Absentee_Voting + Same_Day_Registration + 
            Strict_Voter_ID_Required + 
            Black + Native_American + Asian + Hispanic + Multiracial + 
            Female + 
            Campaign_Competitiveness +
            Online_Registration_Allowed, 
          data=d2.1, family = binomial(link = "logit"))
summary(m1)

# logistic regression - general election, whites
m2 <- glm(Voted ~ 
            In_Person_Early_Voting + Mail_Voting + No_Excuse_Absentee_Voting + Same_Day_Registration + 
            Strict_Voter_ID_Required + 
            White + Female + 
            Campaign_Competitiveness + 
            Online_Registration_Allowed, 
          data=d2.1, family = binomial(link = "logit"))
summary(m2)

# multilevel models

m1.multilevel <- glmer(Voted ~  
                         In_Person_Early_Voting + Mail_Voting + No_Excuse_Absentee_Voting + Same_Day_Registration + 
                         Strict_Voter_ID_Required + 
                         Black + Native_American + Asian + Hispanic + Multiracial + Female + 
                         Campaign_Competitiveness + 
                         Online_Registration_Allowed +
                         (1 | state), 
                       data = d2.1, family = binomial(link = 'logit'))
summary(m1.multilevel)

m2.multilevel <- glmer(Voted ~  
                         In_Person_Early_Voting + Mail_Voting + No_Excuse_Absentee_Voting + Same_Day_Registration + 
                         Strict_Voter_ID_Required + 
                         White + Female + 
                         Campaign_Competitiveness + 
                         Online_Registration_Allowed +
                         (1 | state), 
                       data = d2.1, family = binomial(link = 'logit'))
summary(m2.multilevel)

# logistic regression with robust and bootstrapped standard errors

dd = datadist(d2.1)
options(datadist="dd")

m1.lrm <- lrm(Voted ~  
                In_Person_Early_Voting + Mail_Voting + No_Excuse_Absentee_Voting + Same_Day_Registration + 
                Strict_Voter_ID_Required + 
                Black + Native_American + Asian + Hispanic + Multiracial + Female + 
                Campaign_Competitiveness + 
                Online_Registration_Allowed,
              data = d2.1, x=TRUE, y=TRUE)
set.seed(123) 
m1.bootstrap <- bootcov(m1.lrm, cluster=d2.1$state, B=5000)
m1.robust <- robcov(m1.lrm, cluster=d2.1$state)

stargazer(m1.lrm, m1.robust, m1.bootstrap, m1.multilevel, type = 'text')

##################
# second dataset #
##################

# load jop final.RData from project file

# glance at data 
x %>%
  glimpse() %>%
  summary()

# filter rows from jop final.RData file to age 18-24 
# and completed 'some college'
x %>%
  filter(age %in% c(18,19,20,21,22,23,24)) %>%
  filter(educ == 'Some college') %>%
  {.} -> d1

# tidy dataset
d1 %>%
  mutate(Female = ifelse(male == 1, 0, 1)) %>%
  select(Voted = votegenval, White = white, Black = black, Hispanic = hispanic, Asian = asian, Multiracial = mixedrace, 
         Foreign_Born = foreignb, Income = inc, Female, Days_Before_Election = days_before_election,
         In_Person_Early_Voting = early_in_person, Mail_Voting = vote_by_mail, 
         No_Excuse_Absentee_Voting = no_excuse_absence_, Strict_Voter_ID_Required = stricty, New_Strict_Voter_ID_Required = newstrict, 
         Presidential_Election_Year = presidentialelectionyear, Gubernatorial_Election_Year = gubernatorialelectionyear,
         Senate_Election_Year = senateelectionyear, Campaign_Competitiveness = marginpnew,
         weight, state) %>%
  {.} -> d1

# remove observations with missing Voted values 
d1 <- d1 %>%
  filter(!is.na(Voted))

# logistic regression - general election, minorites
m3 <- glm(Voted ~ 
            In_Person_Early_Voting + Mail_Voting +  No_Excuse_Absentee_Voting + Days_Before_Election + 
            Strict_Voter_ID_Required + 
            Black + Asian + Hispanic + Multiracial + 
            Female +
            Campaign_Competitiveness +
            Income +  
            Presidential_Election_Year + Gubernatorial_Election_Year + Senate_Election_Year,
          data = d1, family = 'binomial')
summary(m3)

# logistic regression - general election, whites
m4 <- glm(Voted ~ 
            In_Person_Early_Voting + Mail_Voting +  No_Excuse_Absentee_Voting + Days_Before_Election + 
            Strict_Voter_ID_Required + 
            White + 
            Female +
            Campaign_Competitiveness +
            Income +  
            Presidential_Election_Year + Gubernatorial_Election_Year + Senate_Election_Year,
          data = d1, family = 'binomial')
summary(m4)

# ------------------------

# multilevel models

m3.multilevel <- glmer(Voted ~  
                         In_Person_Early_Voting + Mail_Voting +  No_Excuse_Absentee_Voting + Days_Before_Election + 
                         Strict_Voter_ID_Required + 
                         Black + Asian + Hispanic + Multiracial + 
                         Female +
                         Campaign_Competitiveness +
                         Income +  
                         Presidential_Election_Year + Gubernatorial_Election_Year + Senate_Election_Year +
                         (1 | state), 
                       data = d1, family = binomial(link = 'logit'))
summary(m3.multilevel)

m4.multilevel <- glmer(Voted ~ 
                         In_Person_Early_Voting + Mail_Voting +  No_Excuse_Absentee_Voting + Days_Before_Election + 
                         Strict_Voter_ID_Required + 
                         White + 
                         Female +
                         Campaign_Competitiveness +
                         Income +  
                         Presidential_Election_Year + Gubernatorial_Election_Year + Senate_Election_Year +
                         (1 | state), 
                       data = d1, family = binomial(link = 'logit'))
summary(m4.multilevel)

# ------------------------

# logistic regression with clustered and bootstrapped standard errors

# filter to state != na
# NOTE: robust clustered standard errors cannot be calculated
#       with missing state values 

d1 <- d1 %>%
  filter(!is.na(state))

dd = datadist(d1)
options(datadist="dd")

m3.lrm <- lrm(Voted ~ 
                In_Person_Early_Voting + Mail_Voting +  No_Excuse_Absentee_Voting + Days_Before_Election + 
                Strict_Voter_ID_Required + 
                Black + Asian + Hispanic + Multiracial + 
                Female +
                Campaign_Competitiveness +
                Income +  
                Presidential_Election_Year + Gubernatorial_Election_Year + Senate_Election_Year,
              data = d1, x=TRUE, y=TRUE)
set.seed(1234)
m3.bootstrap <- bootcov(m3.lrm, cluster=d1$state, B=5000)
m3.cluster <- robcov(m3.lrm, cluster=d1$state)

# table
stargazer(m3.lrm, m3.cluster, m3.bootstrap, m3.multilevel, type = 'text')







