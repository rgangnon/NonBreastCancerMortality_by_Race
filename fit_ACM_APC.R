library(tidyverse)
library(mgcv)

MortalityData_All <- AllRaces %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="All",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

ACM_APC_AllRaces <- fit_poisson_APC(MortalityData_All$All_Deaths,MortalityData_All$Population,
                                    age=MortalityData_All$Age,period=MortalityData_All$Year,rate_scale=1,
                                    ages=0:84,periods=1968:2018,cohorts=1888:2018,trace=TRUE)

MortalityData_White <- Whites %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="White",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_White$ExpDeaths <- exp(predict(ACM_APC_AllRaces$gam_fit,
                                             MortalityData_White %>% mutate(age=Age,period=Year,cohort=Cohort)))*
  MortalityData_White$Population

ACM_APC_White <- fit_poisson_APC(MortalityData_White$All_Deaths,MortalityData_White$ExpDeaths,
                                 age=MortalityData_White$Age,period=MortalityData_White$Year,rate_scale=1,
                                 ages=0:84,periods=1968:2018,cohorts=1888:2018,trace=TRUE)

MortalityData_Black <- Blacks %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Black",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_Black$ExpDeaths <- exp(predict(ACM_APC_AllRaces$gam_fit,
                                             MortalityData_Black %>% mutate(age=Age,period=Year,cohort=Cohort)))*
  MortalityData_Black$Population

ACM_APC_Black <- fit_poisson_APC(MortalityData_Black$All_Deaths,MortalityData_Black$ExpDeaths,
                                 age=MortalityData_Black$Age,period=MortalityData_Black$Year,rate_scale=1,
                                 ages=0:84,periods=1968:2018,cohorts=1888:2018,trace=TRUE)

MortalityData_Asian <- Asians %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Asian",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_Asian$ExpDeaths <- exp(predict(ACM_APC_AllRaces$gam_fit,
                                             MortalityData_Asian %>% mutate(age=Age,period=Year,cohort=Cohort)))*
  MortalityData_Asian$Population

ACM_APC_Asian <- fit_poisson_APC(MortalityData_Asian$All_Deaths,MortalityData_Asian$ExpDeaths,
                                 age=MortalityData_Asian$Age,period=MortalityData_Asian$Year,rate_scale=1,
                                 ages=0:84,periods=1999:2018,cohorts=1915:2018,trace=TRUE)


MortalityData_Hispanic <- Hispanics %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Hispanic",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_Hispanic$ExpDeaths <- exp(predict(ACM_APC_AllRaces$gam_fit,
                                                MortalityData_Hispanic %>% mutate(age=Age,period=Year,cohort=Cohort)))*
  MortalityData_Hispanic$Population

ACM_APC_Hispanic <- fit_poisson_APC(MortalityData_Hispanic$All_Deaths,MortalityData_Hispanic$ExpDeaths,
                                 age=MortalityData_Hispanic$Age,period=MortalityData_Hispanic$Year,rate_scale=1,
                                 ages=0:84,periods=1999:2018,cohorts=1915:2018,trace=TRUE)

MortalityData_NHWhite <- NonHispanicWhites %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Non-Hispanic White",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_NHWhite$ExpDeaths <- exp(predict(ACM_APC_AllRaces$gam_fit,
                                               MortalityData_NHWhite %>% mutate(age=Age,period=Year,cohort=Cohort))+
                                       predict(ACM_APC_White$gam_fit,
                                               MortalityData_NHWhite %>% mutate(age=Age,period=Year,cohort=Cohort)))*       
  MortalityData_NHWhite$Population

ACM_APC_NHWhite <- fit_poisson_APC(MortalityData_NHWhite$All_Deaths,MortalityData_NHWhite$ExpDeaths,
                                    age=MortalityData_NHWhite$Age,period=MortalityData_NHWhite$Year,rate_scale=1,
                                    ages=0:84,periods=1999:2018,cohorts=1915:2018,trace=TRUE)

MortalityData_NHBlack <- NonHispanicBlacks %>% 
  rename(Year=year,Age=age,BC_Deaths=bc_deaths,All_Deaths=deaths,Population=population) %>% 
  mutate(Race="Non-Hispanic Black",Cohort=Year-Age,NBC_Deaths=All_Deaths-BC_Deaths,Percent_BC=BC_Deaths/All_Deaths)

MortalityData_NHBlack$ExpDeaths <- exp(predict(ACM_APC_AllRaces$gam_fit,
                                               MortalityData_NHBlack %>% mutate(age=Age,period=Year,cohort=Cohort))+
                                         predict(ACM_APC_Black$gam_fit,
                                                 MortalityData_NHBlack %>% mutate(age=Age,period=Year,cohort=Cohort)))*       
  MortalityData_NHBlack$Population

ACM_APC_NHBlack <- fit_poisson_APC(MortalityData_NHBlack$All_Deaths,MortalityData_NHBlack$ExpDeaths,
                                   age=MortalityData_NHBlack$Age,period=MortalityData_NHBlack$Year,rate_scale=1,
                                   ages=0:84,periods=1999:2018,cohorts=1915:2018,trace=TRUE)