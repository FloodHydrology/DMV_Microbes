#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Hydrologic Regime and Microbrial Dynamics
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 3/4/2020
#Purpose: Develop hydrologic regime metrics for Microbrial Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(lubridate)
library(tidyverse)

#output location
dir<-"/nfs/palmer-group-data/Choptank/Nate/DepthToWaterTable/"

#Read data
df<-read_csv('data/DepthToWaterTable.csv')

#Select SC sites
df<-df %>% filter(str_detect(station,'SC'))

#Identify threshold of interest
threshold<-0.3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download data--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Estimate Annual Metrics----------------------------------------------------
#Sort based on site & station
annual <- df %>% arrange(wetland, station, day)

#Create collumn with bianary indicator of saturation
annual<-annual %>% mutate(inun = if_else(d_n<threshold, 1,0))

#Identify individual periods of saturation
annual<-annual %>% 
  mutate(event = if_else(wetland == lead(wetland) &
                          station==lead(station) &
                            inun == 1 & 
                              lead(inun) == 0, 
                          1, 0))

#Summarise Data
annual<-annual %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(max_depth_m = max(d_n), 
            mean_depth_m = mean(d_n), 
            median_depth_m = median(d_n), 
            min_depth_m = min(d_n), 
            dur_day = sum(inun),
            n_events = sum(event))

#2.2 Estimate Monthly Metrics---------------------------------------------------
#Sort based on site & station
monthly <- df %>% arrange(wetland, station, day)

#Create collumn with bianary indicator of saturation
monthly<-monthly %>% mutate(inun = if_else(d_n<threshold, 1,0))

#Identify individual events of saturation
monthly<-monthly %>% mutate(event = if_else(wetland == lead(wetland) &
                                            station==lead(station) &
                                            inun == 1 & 
                                            lead(inun) == 0, 
                                          1, 0))

#Filter to November!
monthly<-monthly %>% mutate(month = lubridate::month(day)) %>% filter(month==11)

#Summarise data
monthly<-monthly %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(max_depth_m = max(d_n), 
            mean_depth_m = mean(d_n), 
            median_depth_m = median(d_n), 
            min_depth_m = min(d_n), 
            dur_day = sum(inun),
            n_events = sum(event))

#Add 1 event if november ended in saturation...
monthly<-monthly %>% mutate(n_events = if_else(dur_day>0 & n_events==0, 1, n_events))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Export data----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(annual, "data//annual_metrics.csv")
write_csv(monthly, "data//monthly_metrics.csv")





