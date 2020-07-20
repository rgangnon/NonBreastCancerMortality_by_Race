library(tidyverse)
library(mgcv)

BC_All_Estimates <- tibble(age=rep(0:99,length(1968:2018)),
                           period=rep(1968:2018,rep(length(0:99),length(1968:2018)))) %>% 
  mutate(cohort=period-age)

BC_All_Estimates <- predict_APC(BC_All_APC_AllRaces,BC_All_Estimates) %>% 
  rename(lp_all=lp,se_lp_all=se_lp)

BC_All_Estimates <- predict_APC(BC_All_APC_White,BC_All_Estimates) %>% 
  rename(lp_white=lp,se_lp_white=se_lp)

BC_All_Estimates <- predict_APC(BC_All_APC_Black,BC_All_Estimates) %>% 
  rename(lp_black=lp,se_lp_black=se_lp)

BC_All_Estimates <- predict_APC(BC_All_APC_Asian,BC_All_Estimates) %>% 
  rename(lp_asian=lp,se_lp_asian=se_lp)

BC_All_Estimates <- predict_APC(BC_All_APC_Hispanic,BC_All_Estimates) %>% 
  rename(lp_hisp=lp,se_lp_hisp=se_lp)

BC_All_Estimates <- predict_APC(BC_All_APC_NHWhite,BC_All_Estimates) %>% 
  rename(lp_nhwhite=lp,se_lp_nhwhite=se_lp)

BC_All_Estimates <- predict_APC(BC_All_APC_NHBlack,BC_All_Estimates) %>% 
  rename(lp_nhblack=lp,se_lp_nhblack=se_lp)

BC_All_Estimates_AllRaces <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="All Races",Logit_BC_All=lp_all,SE_Logit_BC_All=se_lp_all) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates_White <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="White",Logit_BC_All=lp_all+lp_white,SE_Logit_BC_All=sqrt(se_lp_all^2+se_lp_white^2)) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates_Black <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Black",Logit_BC_All=lp_all+lp_black,SE_Logit_BC_All=sqrt(se_lp_all^2+se_lp_black^2)) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates_Asian <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Asian",Logit_BC_All=lp_all+lp_asian,SE_Logit_BC_All=sqrt(se_lp_all^2+se_lp_asian^2)) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates_Hispanic <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Hispanic",Logit_BC_All=lp_all+lp_hisp,SE_Logit_BC_All=sqrt(se_lp_all^2+se_lp_hisp^2)) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates_NHWhite <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Non-Hispanic White",Logit_BC_All=lp_all+lp_white+lp_nhwhite,
         SE_Logit_BC_All=sqrt(se_lp_all^2+se_lp_white^2+se_lp_nhwhite^2)) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates_NHBlack <- BC_All_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Non-Hispanic Black",Logit_BC_All=lp_all+lp_black+lp_nhblack,
         SE_Logit_BC_All=sqrt(se_lp_all^2+se_lp_black^2+se_lp_nhblack^2)) %>% 
  select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All)

BC_All_Estimates <- rbind(BC_All_Estimates_AllRaces,
                          BC_All_Estimates_Asian,
                          BC_All_Estimates_Black,
                          BC_All_Estimates_Hispanic,
                          BC_All_Estimates_NHBlack,
                          BC_All_Estimates_NHWhite,
                          BC_All_Estimates_White) %>% 
  mutate(BC_All=alogit(Logit_BC_All),
         BC_All_LCL=alogit(Logit_BC_All-1.96*SE_Logit_BC_All),
         BC_All_UCL=alogit(Logit_BC_All+1.96*SE_Logit_BC_All))

rm(BC_All_Estimates_AllRaces,
   BC_All_Estimates_Asian,
   BC_All_Estimates_Black,
   BC_All_Estimates_Hispanic,
   BC_All_Estimates_NHBlack,
   BC_All_Estimates_NHWhite,
   BC_All_Estimates_White)
