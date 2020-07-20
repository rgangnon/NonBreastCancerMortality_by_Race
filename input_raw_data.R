library(tidyverse)

#create a single dataset for BC deaths and deaths by age and year for all races

tmp<-read_delim("RawData/All Deaths, 1968-1978, All Races.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths,population) 

tmp2<-read_delim("RawData/All Deaths, 1979-1998, All Races.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths,population) 

tmp3 <- read_delim("RawData/All Deaths, 1999-2018, All Races.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

AllDeaths_AllRaces <- rbind(tmp,tmp2,tmp3)
rm(tmp,tmp2,tmp3)


tmp<-read_delim("RawData/Breast Cancer Deaths, 1968-1978, All Races.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths) 

tmp2<-read_delim("RawData/Breast Cancer Deaths, 1979-1998, All Races.txt",delim="\t",
                 col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                 skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths) 

tmp3 <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, All Races.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths)

BreastCancerDeaths_AllRaces <- rbind(tmp,tmp2,tmp3) %>% rename(bc_deaths=deaths)
rm(tmp,tmp2,tmp3)

AllRaces <- BreastCancerDeaths_AllRaces %>% full_join(AllDeaths_AllRaces)
rm(AllDeaths_AllRaces,BreastCancerDeaths_AllRaces)

#create a single dataset for BC deaths and deaths by age and year for whites

tmp<-read_delim("RawData/All Deaths, 1968-1978, White.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths,population) 

tmp2<-read_delim("RawData/All Deaths, 1979-1998, White.txt",delim="\t",
                 col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                 skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths,population) 

tmp3 <- read_delim("RawData/All Deaths, 1999-2018, White.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

AllDeaths_Whites <- rbind(tmp,tmp2,tmp3)
rm(tmp,tmp2,tmp3)


tmp<-read_delim("RawData/Breast Cancer Deaths, 1968-1978, White.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths) 

tmp2<-read_delim("RawData/Breast Cancer Deaths, 1979-1998, White.txt",delim="\t",
                 col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                 skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths) 

tmp3 <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, White.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths)

BreastCancerDeaths_Whites <- rbind(tmp,tmp2,tmp3) %>% rename(bc_deaths=deaths)
rm(tmp,tmp2,tmp3)

Whites <- BreastCancerDeaths_Whites %>% full_join(AllDeaths_Whites)
rm(AllDeaths_Whites,BreastCancerDeaths_Whites)


#create a single dataset for BC deaths and deaths by age and year for blacks

tmp<-read_delim("RawData/All Deaths, 1968-1978, Black.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths,population) 

tmp2<-read_delim("RawData/All Deaths, 1979-1998, Black.txt",delim="\t",
                 col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                 skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths,population) 

tmp3 <- read_delim("RawData/All Deaths, 1999-2018, Black.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

AllDeaths_Blacks <- rbind(tmp,tmp2,tmp3)
rm(tmp,tmp2,tmp3)


tmp<-read_delim("RawData/Breast Cancer Deaths, 1968-1978, Black.txt",delim="\t",
                col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                skip=1) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths) 

tmp2<-read_delim("RawData/Breast Cancer Deaths, 1979-1998, Black.txt",delim="\t",
                 col_names=c("notes","year","year_code","age_group","age_group_code","deaths","population","crude_rate"),
                 skip=1) %>% 
  filter(is.na(notes),!(age_group_code=="NS"),!(age_group_code=="85+")) %>% 
  mutate(age=ifelse(age_group_code=="1",0,
                    ifelse(age_group_code=="1-4",2.5,
                           ifelse(age_group_code=="5-9",7.5,
                                  ifelse(age_group_code=="10-14",12,
                                         ifelse(age_group_code=="15-19",17.5,
                                                ifelse(age_group_code=="20-24",22,
                                                       ifelse(age_group_code=="25-34",29.5,
                                                              ifelse(age_group_code=="35-44",39.5,
                                                                     ifelse(age_group_code=="45-54",49.5,
                                                                            ifelse(age_group_code=="55-64",59.5,
                                                                                   ifelse(age_group_code=="65-74",69.5,
                                                                                          ifelse(age_group_code=="75-84",79.5,
                                                                                                 NA))))))))))))) %>% 
  select(year,age,deaths) 

tmp3 <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, Black.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths)

BreastCancerDeaths_Blacks <- rbind(tmp,tmp2,tmp3) %>% rename(bc_deaths=deaths)
rm(tmp,tmp2,tmp3)

Blacks <- BreastCancerDeaths_Blacks %>% full_join(AllDeaths_Blacks)
rm(AllDeaths_Blacks,BreastCancerDeaths_Blacks)

#input 1999-2018 data for other race/ethnicity categories

AllDeaths_Hispanics <- read_delim("RawData/All Deaths, 1999-2018, Hispanic.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

BreastCancerDeaths_Hispanics <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, Hispanic.txt",delim="\t",
                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                   skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths) %>% 
  rename(bc_deaths=deaths)

Hispanics <- BreastCancerDeaths_Hispanics %>% full_join(AllDeaths_Hispanics)
rm(AllDeaths_Hispanics,BreastCancerDeaths_Hispanics)

AllDeaths_NonHispanicWhites <- read_delim("RawData/All Deaths, 1999-2018, NonHispanicWhite.txt",delim="\t",
                                          col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                                          skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

BreastCancerDeaths_NonHispanicWhites <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, NonHispanicWhite.txt",delim="\t",
                                           col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                                           skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths) %>% 
  rename(bc_deaths=deaths)

NonHispanicWhites <- BreastCancerDeaths_NonHispanicWhites %>% full_join(AllDeaths_NonHispanicWhites)
rm(AllDeaths_NonHispanicWhites,BreastCancerDeaths_NonHispanicWhites)

AllDeaths_NonHispanicBlacks <- read_delim("RawData/All Deaths, 1999-2018, NonHispanicBlack.txt",delim="\t",
                                          col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                                          skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

BreastCancerDeaths_NonHispanicBlacks <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, NonHispanicBlack.txt",delim="\t",
                                                   col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                                                   skip=1) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths) %>% 
  rename(bc_deaths=deaths)

NonHispanicBlacks <- BreastCancerDeaths_NonHispanicBlacks %>% full_join(AllDeaths_NonHispanicBlacks)
rm(AllDeaths_NonHispanicBlacks,BreastCancerDeaths_NonHispanicBlacks)

AllDeaths_Asians <- read_delim("RawData/All Deaths, 1999-2018, Asian.txt",delim="\t",
                               col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                               skip=1,na=c("","NA","Not Applicable")) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths,population)

BreastCancerDeaths_Asians <- read_delim("RawData/Breast Cancer Deaths, 1999-2018, Asian.txt",delim="\t",
                                        col_names=c("notes","year","year_code","age_group","age","deaths","population","crude_rate"),
                                        skip=1) %>% 
  filter(is.na(notes),!(age=="NS"),!(age=="100")) %>% 
  mutate(age=as.numeric(age)) %>% 
  select(year,age,deaths) %>% 
  rename(bc_deaths=deaths)

Asians <- BreastCancerDeaths_Asians %>% full_join(AllDeaths_Asians)
rm(AllDeaths_Asians,BreastCancerDeaths_Asians)

save.image()
