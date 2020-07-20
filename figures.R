library(tidyverse)

tiff("Figures/Figure1.tiff",height=6,width=8,units="in",res=300,pointsize=12)
BCM_Estimates %>% filter(Period%in%seq(1968,2018,by=5),Age>=30,Age<85,!(Race=="White"),!(Race=="Black"),!(Race=="All Races")) %>% 
  ggplot(aes(x=Age,y=BCM*100000,ymin=BCM_LCL*100000,ymax=BCM_UCL*100000,fill=Race)) + 
  geom_ribbon(color=NA,alpha=.5) +
  facet_wrap(~Period) + theme_bw() +
  scale_y_log10(breaks=c(1,3,10,30,100)) +
  scale_fill_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                    labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Age (yrs)") + ylab("Breast Cancer Mortality (per 100,000 Women)") + 
  theme(legend.position=c(.875,1/6)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) 
dev.off()

tiff("Figures/Figure2.tiff",height=6,width=8,units="in",res=300,pointsize=12)
BC_All_Estimates %>% filter(Period%in%seq(1968,2018,by=5),Age>=20,!(Race=="White"),!(Race=="Black"),!(Race=="All Races")) %>% 
  ggplot(aes(x=Age,y=BC_All*100,ymin=BC_All_LCL*100,ymax=BC_All_UCL*100,fill=Race)) + 
  geom_ribbon(color=NA,alpha=.5) +
  facet_wrap(~Period) + theme_bw() +
  scale_fill_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                    labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Age (yrs)") + ylab("Percent of Deaths Due to Breast Cancer") + 
  theme(legend.position=c(.875,1/6)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) 
dev.off()

tiff("Figures/SupFigure1.tiff",height=6,width=8,units="in",res=300,pointsize=12)
ACM_Estimates %>% filter(Period%in%seq(1968,2018,by=5),Age>=30,Age<85,!(Race=="White"),!(Race=="Black"),!(Race=="All Races")) %>% 
  ggplot(aes(x=Age,y=ACM*100000,ymin=ACM_LCL*100000,ymax=ACM_UCL*100000,fill=Race)) + 
  geom_ribbon(color=NA,alpha=.5) +
  facet_wrap(~Period) + theme_bw() +
  scale_y_log10(breaks=c(30,100,300,1000,3000,10000)) +
  scale_fill_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                    labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Age (yrs)") + ylab("All Cause Mortality (per 100,000 Women)") + 
  theme(legend.position=c(.875,1/6)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) 
dev.off()

tiff("Figures/SupFigure2.tiff",height=6,width=8,units="in",res=300,pointsize=12)
NBCM_Estimates %>% filter(Period%in%seq(1968,2018,by=5),Age>=30,Age<85,!(Race=="White"),!(Race=="Black"),!(Race=="All Races")) %>% 
  ggplot(aes(x=Age,y=NBCM*100000,ymin=NBCM_LCL*100000,ymax=NBCM_UCL*100000,fill=Race)) + 
  geom_ribbon(color=NA,alpha=.5) +
  facet_wrap(~Period) + theme_bw() +
  scale_y_log10(breaks=c(30,100,300,1000,3000,10000)) +
  scale_fill_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                    labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Age (yrs)") + ylab("Non-Breast Cancer Mortality (per 100,000 Women)") + 
  theme(legend.position=c(.875,1/6)) +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) 
dev.off()

tiff("Figures/SupFigure3.tiff",height=5,width=8,units="in",res=300,pointsize=12)
BCM_AgeAdj %>% filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>% 
  ggplot(aes(x=Period,y=AgeAdj_BCM*100000,color=Race)) + geom_line(alpha=.5,size=1) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1968,2018,by=10)) +
  scale_y_log10() +
  scale_color_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                  labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Year") + 
  ylab("Breast Cancer Mortality (per 100,000 Women)\nAge Adjusted to the Female Population Age 18-84 in 2018") +
  theme(legend.position="bottom") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )    
dev.off()

tiff("Figures/SupFigure4.tiff",height=5,width=8,units="in",res=300,pointsize=12)
BC_All_AgeAdj %>% filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>% 
  ggplot(aes(x=Period,y=AgeAdj_BC_All*100,color=Race)) + geom_line(alpha=.5,size=1) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1968,2018,by=10)) +
  scale_color_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                     labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Year") + 
  ylab("Percent of Deaths Due to Breast Cancer\nAge Adjusted to Female Deaths Age 18-84 in 2018") +
  theme(legend.position="bottom") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )    
dev.off()

tiff("Figures/SupFigure5.tiff",height=5,width=8,units="in",res=300,pointsize=12)
ACM_AgeAdj %>% filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>% 
  ggplot(aes(x=Period,y=AgeAdj_ACM*100000,color=Race)) + geom_line(alpha=.5,size=1) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1968,2018,by=10)) +
  scale_y_log10(breaks=c(300,450,600,900,1200,1700)) +
  scale_color_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                     labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Year") + 
  ylab("All Cause Mortality (per 100,000 Women)\nAge Adjusted to the Female Population Age 18-84 in 2018") +
  theme(legend.position="bottom") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )    
dev.off()

tiff("Figures/SupFigure6.tiff",height=5,width=8,units="in",res=300,pointsize=12)
NBCM_AgeAdj %>% filter(Race%in%c("Asian","Hispanic","Non-Hispanic Black","Non-Hispanic White")) %>% 
  ggplot(aes(x=Period,y=AgeAdj_NBCM*100000,color=Race)) + geom_line(alpha=.5,size=1) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1968,2018,by=10)) +
  scale_y_log10(breaks=c(300,450,600,900,1200,1700)) +
  scale_color_manual("",values=c("Red","DarkOrange","DarkGreen","Blue"),
                     labels=c("Asian/Pacific Islander","Hispanic","Non-Hispanic Black","Non-Hispanic White"))+
  xlab("Year") + 
  ylab("Non-Breast Cancer Mortality (per 100,000 Women)\nAge Adjusted to the Female Population Age 18-84 in 2018") +
  theme(legend.position="bottom") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )    
dev.off()
