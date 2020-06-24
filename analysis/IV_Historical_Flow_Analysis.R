#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Histoical Anlaysis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 6/23/2020
#Purpose: Exxamine 2018 water year in reference to USGS record
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Workspace---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#load appropriate packages
library(lubridate)
library(tidyverse)
library(dataRetrieval)

#load data
df<-readNWISdv(siteNumbers = '01491000', parameterCd = '00060')  

#Add Water Year
df<-addWaterYear(df)  

#Organize
df<-df %>% 
  as_tibble() %>% 
  select(Timestamp = Date,
         year      = waterYear,
         Q_cfs     = X_00060_00003)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Estimtate Annual Water Yield--------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Calculate Annual Water Yield
runoff<-df %>% 
  #Convert to in/day
  mutate(q_in = Q_cfs/(113*2.788e+7)*12*86400) %>% 
  #Summarise by water year
  group_by(year) %>% 
  summarise(q_in = sum(q_in)) %>% 
  mutate(q_mm = q_in*25.5) %>% 
  #filter to whole years
  filter(year<2020)

#Define 2018 water year
study_year_1<-runoff$q_mm[runoff$year==2018]
study_year_2<-runoff$q_mm[runoff$year==2019]

#Flow Record
ecdf(runoff$q_mm)(study_year_1)
ecdf(runoff$q_mm)(study_year_2)

#Plot
runoff %>% 
  ggplot(aes(q_mm)) +
  geom_vline(aes(xintercept=study_year_1), lty=2,lwd=1.25, col="grey30")+ 
  geom_vline(aes(xintercept=study_year_2), lty=2,lwd=1.25, col="grey30")+ 
  stat_ecdf(geom = 'step') + 
  geom_text(x = 450, y=0.22, label = "WY2018")+
  geom_text(x = 720, y=0.47, label = "WY2019")+
  theme_bw() +
    xlab("Runoff [mm/year]") + 
    ylab("Cumulative Probability") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) 
ggsave("runoff.png", width=3.5, height=3, units="in")
  