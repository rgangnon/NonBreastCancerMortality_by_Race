library(tidyverse)
library(mgcv)

logit <- function(p) log(p)-log(1-p)
alogit <- function(x) 1/(1+exp(-x))

#

MortalityData_All <- AllRaces %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="All",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

BC_All_APC_AllRaces <- fit_binomial_APC(MortalityData_All$BC_Deaths,MortalityData_All$All_Deaths,
                                        age=MortalityData_All$Age,period=MortalityData_All$Year,
                                        ages=0:99,periods=1968:2018,cohorts=1888:2018,trace=TRUE)

MortalityData_White <- Whites %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="White",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_White$Pred_All <- predict(BC_All_APC_AllRaces$gam_fit,
                                        MortalityData_White %>% mutate(age=Age,period=Year,cohort=Cohort))

BC_All_APC_White <- fit_binomial_APC(MortalityData_White$BC_Deaths,MortalityData_White$All_Deaths,
                                     age=MortalityData_White$Age,period=MortalityData_White$Year,
                                     offset=MortalityData_White$Pred_All,
                                     ages=0:99,periods=1968:2018,cohorts=1888:2018,trace=TRUE)

MortalityData_Black <- Blacks %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Black",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_Black$Pred_All <- predict(BC_All_APC_AllRaces$gam_fit,
                                        MortalityData_Black %>% mutate(age=Age,period=Year,cohort=Cohort))

BC_All_APC_Black <- fit_binomial_APC(MortalityData_Black$BC_Deaths,MortalityData_Black$All_Deaths,
                                     age=MortalityData_Black$Age,period=MortalityData_Black$Year,
                                     offset=MortalityData_Black$Pred_All,
                                     ages=0:99,periods=1968:2018,cohorts=1888:2018,trace=TRUE)

MortalityData_Asian <- Asians %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Asian",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_Asian$Pred_All <- predict(BC_All_APC_AllRaces$gam_fit,
                                        MortalityData_Asian %>% mutate(age=Age,period=Year,cohort=Cohort))

BC_All_APC_Asian <- fit_binomial_APC(MortalityData_Asian$BC_Deaths,MortalityData_Asian$All_Deaths,
                                     age=MortalityData_Asian$Age,period=MortalityData_Asian$Year,
                                     offset=MortalityData_Asian$Pred_All,
                                     ages=0:99,periods=1999:2018,cohorts=1900:2018,trace=TRUE)

MortalityData_Hispanic <- Hispanics %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Hispanic",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_Hispanic$Pred_All <- predict(BC_All_APC_AllRaces$gam_fit,
                                           MortalityData_Hispanic %>% mutate(age=Age,period=Year,cohort=Cohort))

BC_All_APC_Hispanic <- fit_binomial_APC(MortalityData_Hispanic$BC_Deaths,MortalityData_Hispanic$All_Deaths,
                                        age=MortalityData_Hispanic$Age,period=MortalityData_Hispanic$Year,
                                        offset=MortalityData_Hispanic$Pred_All,
                                        ages=0:99,periods=1999:2018,cohorts=1900:2018,trace=TRUE)

MortalityData_NHWhite <- NonHispanicWhites %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Non-Hispanic White",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_NHWhite$Pred_All <- predict(BC_All_APC_AllRaces$gam_fit,
                                          MortalityData_NHWhite %>% mutate(age=Age,period=Year,cohort=Cohort)) +
                                  predict(BC_All_APC_White$gam_fit,
                                          MortalityData_NHWhite %>% mutate(age=Age,period=Year,cohort=Cohort))

BC_All_APC_NHWhite <- fit_binomial_APC(MortalityData_NHWhite$BC_Deaths,MortalityData_NHWhite$All_Deaths,
                                       age=MortalityData_NHWhite$Age,period=MortalityData_NHWhite$Year,
                                       offset=MortalityData_NHWhite$Pred_All,
                                       ages=0:99,periods=1999:2018,cohorts=1900:2018,trace=TRUE)

MortalityData_NHBlack <- NonHispanicBlacks %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Non-Hispanic Black",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_NHBlack$Pred_All <- predict(BC_All_APC_AllRaces$gam_fit,
                                          MortalityData_NHBlack %>% mutate(age=Age,period=Year,cohort=Cohort)) +
  predict(BC_All_APC_Black$gam_fit,
          MortalityData_NHBlack %>% mutate(age=Age,period=Year,cohort=Cohort))

BC_All_APC_NHBlack <- fit_binomial_APC(MortalityData_NHBlack$BC_Deaths,MortalityData_NHBlack$All_Deaths,
                                       age=MortalityData_NHBlack$Age,period=MortalityData_NHBlack$Year,
                                       offset=MortalityData_NHBlack$Pred_All,
                                       ages=0:99,periods=1999:2018,cohorts=1900:2018,trace=TRUE)
