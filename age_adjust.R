library(tidyverse)

Standard <- AllRaces %>% filter(year==2018) %>% select(age,deaths,population) %>% 
  mutate(deaths=ifelse(age%in%18:84,deaths,0),population=ifelse(age%in%18:84,population,0)) %>% 
  mutate(p_deaths=deaths/sum(deaths),p_population=population/sum(population)) %>% 
  rename(Age=age) %>% select(Age,p_deaths,p_population)

BC_All_AgeAdj <- BC_All_Estimates %>% left_join(Standard) %>% 
  group_by(Period,Race) %>% summarize(AgeAdj_BC_All=sum(p_deaths*BC_All))

ACM_AgeAdj <- ACM_Estimates %>% left_join(Standard) %>% 
  group_by(Period,Race) %>% summarize(AgeAdj_ACM=sum(p_population*ACM))

BCM_AgeAdj <- BCM_Estimates %>% left_join(Standard) %>% 
  group_by(Period,Race) %>% summarize(AgeAdj_BCM=sum(p_population*BCM))

NBCM_AgeAdj <- NBCM_Estimates %>% left_join(Standard) %>% 
  group_by(Period,Race) %>% summarize(AgeAdj_NBCM=sum(p_population*NBCM))
