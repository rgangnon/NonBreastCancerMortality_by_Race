library(tidyverse)
library(mgcv)

ACM_Estimates <- tibble(age=rep(0:99,length(1968:2018)),
                        period=rep(1968:2018,rep(length(0:99),length(1968:2018)))) %>% 
  mutate(cohort=period-age)

ACM_Estimates <- predict_APC(ACM_APC_AllRaces,ACM_Estimates) %>% 
  rename(lp_all=lp,se_lp_all=se_lp)

ACM_Estimates <- predict_APC(ACM_APC_White,ACM_Estimates) %>% 
  rename(lp_white=lp,se_lp_white=se_lp)

ACM_Estimates <- predict_APC(ACM_APC_Black,ACM_Estimates) %>% 
  rename(lp_black=lp,se_lp_black=se_lp)

ACM_Estimates <- predict_APC(ACM_APC_Asian,ACM_Estimates) %>% 
  rename(lp_asian=lp,se_lp_asian=se_lp)

ACM_Estimates <- predict_APC(ACM_APC_Hispanic,ACM_Estimates) %>% 
  rename(lp_hisp=lp,se_lp_hisp=se_lp)

ACM_Estimates <- predict_APC(ACM_APC_NHWhite,ACM_Estimates) %>% 
  rename(lp_nhwhite=lp,se_lp_nhwhite=se_lp)

ACM_Estimates <- predict_APC(ACM_APC_NHBlack,ACM_Estimates) %>% 
  rename(lp_nhblack=lp,se_lp_nhblack=se_lp)

ACM_Estimates_AllRaces <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="All Races",Log_ACM=lp_all,SE_Log_ACM=se_lp_all) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates_White <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="White",Log_ACM=lp_all+lp_white,SE_Log_ACM=sqrt(se_lp_all^2+se_lp_white^2)) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates_Black <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Black",Log_ACM=lp_all+lp_black,SE_Log_ACM=sqrt(se_lp_all^2+se_lp_black^2)) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates_Asian <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Asian",Log_ACM=lp_all+lp_asian,SE_Log_ACM=sqrt(se_lp_all^2+se_lp_asian^2)) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates_Hispanic <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Hispanic",Log_ACM=lp_all+lp_hisp,SE_Log_ACM=sqrt(se_lp_all^2+se_lp_hisp^2)) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates_NHWhite <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Non-Hispanic White",Log_ACM=lp_all+lp_white+lp_nhwhite,
         SE_Log_ACM=sqrt(se_lp_all^2+se_lp_white^2+se_lp_nhwhite^2)) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates_NHBlack <- ACM_Estimates %>% 
  rename(Age=age,Period=period) %>% 
  mutate(Race="Non-Hispanic Black",Log_ACM=lp_all+lp_black+lp_nhblack,
         SE_Log_ACM=sqrt(se_lp_all^2+se_lp_black^2+se_lp_nhblack^2)) %>% 
  select(Age,Period,Race,Log_ACM,SE_Log_ACM)

ACM_Estimates <- rbind(ACM_Estimates_AllRaces,
                          ACM_Estimates_Asian,
                          ACM_Estimates_Black,
                          ACM_Estimates_Hispanic,
                          ACM_Estimates_NHBlack,
                          ACM_Estimates_NHWhite,
                          ACM_Estimates_White) %>% 
  mutate(ACM=exp(Log_ACM),
         ACM_LCL=exp(Log_ACM-1.96*SE_Log_ACM),
         ACM_UCL=exp(Log_ACM+1.96*SE_Log_ACM))

rm(ACM_Estimates_AllRaces,
   ACM_Estimates_Asian,
   ACM_Estimates_Black,
   ACM_Estimates_Hispanic,
   ACM_Estimates_NHBlack,
   ACM_Estimates_NHWhite,
   ACM_Estimates_White)

BCM_Estimates <- BC_All_Estimates %>% select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All) %>% 
  full_join(ACM_Estimates %>% select(Age,Period,Race,Log_ACM,SE_Log_ACM)) %>% 
  mutate(Log_BCM=Log_ACM+log(alogit(Logit_BC_All)),
         SE_Log_BCM=sqrt(SE_Log_ACM^2+(1-alogit(Logit_BC_All))^2*SE_Logit_BC_All^2)) %>% 
  mutate(BCM=exp(Log_BCM),
         BCM_LCL=exp(Log_BCM-1.96*SE_Log_BCM),
         BCM_UCL=exp(Log_BCM+1.96*SE_Log_BCM)) %>% 
  select(Age,Period,Race,Log_BCM,SE_Log_BCM,BCM,BCM_LCL,BCM_UCL)

NBCM_Estimates <- BC_All_Estimates %>% select(Age,Period,Race,Logit_BC_All,SE_Logit_BC_All) %>% 
  full_join(ACM_Estimates %>% select(Age,Period,Race,Log_ACM,SE_Log_ACM)) %>% 
  mutate(Log_NBCM=Log_ACM+log(alogit(-Logit_BC_All)),
         SE_Log_NBCM=sqrt(SE_Log_ACM^2+alogit(Logit_BC_All)^2*SE_Logit_BC_All^2)) %>% 
  mutate(NBCM=exp(Log_NBCM),
         NBCM_LCL=exp(Log_NBCM-1.96*SE_Log_NBCM),
         NBCM_UCL=exp(Log_NBCM+1.96*SE_Log_NBCM)) %>% 
  select(Age,Period,Race,Log_NBCM,SE_Log_NBCM,NBCM,NBCM_LCL,NBCM_UCL) 
