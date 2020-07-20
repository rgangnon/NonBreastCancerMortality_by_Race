library(tidyverse)

BCM_Estimates %>% filter(Age%in%seq(30,80,by=10),
                         Period%in%c(1978,1998,2018),
                         Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(BCM=BCM*100000,BCM_LCL=BCM_LCL*100000,BCM_UCL=BCM_UCL*100000) %>%
  select(Age,Period,Race,BCM,BCM_LCL,BCM_UCL) %>%
  arrange(Period,Age,Race) %>% write_csv("Tables/Table1.csv")

BC_All_Estimates %>% filter(Age%in%seq(30,80,by=10),
                         Period%in%c(1978,1998,2018),
                         Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(BC_All=BC_All*100,BC_All_LCL=BC_All_LCL*100,BC_All_UCL=BC_All_UCL*100) %>%
  select(Age,Period,Race,BC_All,BC_All_LCL,BC_All_UCL) %>%
  arrange(Period,Age,Race) %>% write_csv("Tables/Table2.csv")

BC_All_Estimates %>% filter(Age%in%seq(30,80,by=10),
                            Period%in%c(1978,1998,2018),
                            Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(BC_All=BC_All*100,BC_All_LCL=BC_All_LCL*100,BC_All_UCL=BC_All_UCL*100) %>%
  select(Age,Period,Race,BC_All,BC_All_LCL,BC_All_UCL) %>%
  arrange(Period,Age,Race) %>% write_csv("Tables/Table2.csv")

BC_All_Max <- BC_All_Estimates %>% group_by(Race,Period) %>% filter(BC_All==max(BC_All)) %>% select(Race,Period,Age,BC_All)

BC_All_Estimates %>% filter(Period%in%c(1978,1998,2018),
                            Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(BC_All=BC_All*100,BC_All_LCL=BC_All_LCL*100,BC_All_UCL=BC_All_UCL*100) %>%
  group_by(Race,Period) %>%
  filter(BC_All==max(BC_All)) %>%
  select(Period,Race,Age,BC_All,BC_All_LCL,BC_All_UCL) %>%
  arrange(Period,Race) %>% write_csv("Tables/Table3.csv")

BC_All_Estimates %>% filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(BC_All=BC_All*100,BC_All_LCL=BC_All_LCL*100,BC_All_UCL=BC_All_UCL*100) %>%
  group_by(Race) %>%
  filter(BC_All==max(BC_All)) %>%
  select(Race,Period,Age,BC_All,BC_All_LCL,BC_All_UCL) %>%
  arrange(Race) %>% write_csv("Tables/Table3b.csv")

BCM_AgeAdj %>%
  filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(AgeAdj_BCM=AgeAdj_BCM*100000) %>%
  pivot_wider(id=Period,names_from=Race,values_from=AgeAdj_BCM) %>%
  write_csv("Tables/Table4.csv")


BC_All_AgeAdj %>%
  filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>%
  mutate(AgeAdj_BC_All=AgeAdj_BC_All*100) %>%
  pivot_wider(id=Period,names_from=Race,values_from=AgeAdj_BC_All) %>%
  write_csv("Tables/Table5.csv")

print(BC_All_Estimates %>% filter(Period==2018,Race=="Non-Hispanic White")  %>% mutate(White=BC_All) %>% select(Age,White) %>%
  full_join(BC_All_Estimates %>% filter(Period==2018,Race=="Non-Hispanic Black") %>% mutate(Black=BC_All) %>% select(Age,Black) ) %>%
  filter(White < Black),n=100)

print(BC_All_Estimates %>% filter(Period==2018,Race=="Non-Hispanic White")  %>% mutate(White=BC_All) %>% select(Age,White) %>%
        full_join(BC_All_Estimates %>% filter(Period==2018,Race=="Hispanic") %>% mutate(Hispanic=BC_All) %>% select(Age,Hispanic) ) %>%
        filter(White < Hispanic),n=100)

print(BC_All_Estimates %>% filter(Period==2018,Race=="Non-Hispanic White")  %>% mutate(White=BC_All) %>% select(Age,White) %>%
        full_join(BC_All_Estimates %>% filter(Period==2018,Race=="Asian") %>% mutate(Asian=BC_All) %>% select(Age,Asian) ) %>%
        filter(White < Asian),n=100)
