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
#Create collumn with bianary indicator of saturation
annual<-df %>% mutate(inun = if_else(d_n<threshold, 1,0))

#Identify individual periods of saturation
annual<-annual %>% mutate(event = if_else(inun == 1 & lead(inun) == 0, 1, 0))

#Summarise Data
annual<-annual %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(max_depth = max(d_n), 
            mean_depth = mean(d_n), 
            median_depth = median(d_n), 
            min_depth = min(d_n), 
            dur_day = sum(inun),
            n_events = sum(event))

#2.2 Estimate Monthly Metrics---------------------------------------------------
#Create collumn with bianary indicator of saturation
monthly<-df %>% mutate(inun = if_else(d_n<threshold, 1,0))

#Identify individual events of saturation
monthly<-monthly %>% mutate(event = if_else(inun == 1 & lead(inun) == 0, 1, 0))

#Filter to November!
monthly<-monthly %>% mutate(month = lubridate::month(day)) %>% filter(month==11)
  
#Summarise data
monthly<-monthly %>% 
  #Group by wetland and sampling station
  group_by(wetland, station) %>% 
  #Summarise!
  summarise(max_depth = max(d_n), 
            mean_depth = mean(d_n), 
            median_depth = median(d_n), 
            min_depth = min(d_n), 
            dur_day = sum(inun),
            n_events = sum(event))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Export data----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






