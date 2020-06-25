#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Plots
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 6/23/2020
#Purpose: Plot water level data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup Workspace---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory 
remove(list=ls())

#load appropriate packages
library(patchwork)
library(lubridate)
library(tidyverse)

#load data
df<-read_csv("data//waterLevel_cleaned.csv")
depth<-read_csv("data//DepthToWaterTable.csv")
metrics<-read_csv("data//annual_metrics.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Hydrologic Regime Plots-------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Wetland hydrograph~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hyd<-df %>% 
  #seleect wetland well data
  filter(str_detect(site, 'Wetland Well')) %>% 
  #Group by date
  group_by(day) %>% 
  #Summarise water level data
  summarise(
    median = median(waterLevel, na.rm = T),
    lwr    = quantile(waterLevel, 0.25, na.rm = T), 
    upr    = quantile(waterLevel, 0.75, na.rm = T)) %>% 
  #Start ggplot
  ggplot() + 
    geom_hline(aes(yintercept=0), lty=2,lwd=1.25, col="grey30")+  
    geom_ribbon(aes(ymin = lwr, ymax = upr, x = day), 
                fill="#377eb8", 
                col='grey30',
                alpha=0.7) + 
    geom_line(aes(y = median, x = day), col="grey30")+
    theme_bw() + 
      ylab("Wetland Water Level") + 
      xlab(NULL) + 
      scale_x_date(date_labels = "%b") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) 

#Water Level Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dep<-depth %>%
  #Convert depth-to-water-table to water level
  mutate(d_n = -1*d_n) %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect) %>% 
  summarise(median = median(d_n, na.rm=T), 
            upr    = quantile(d_n, 0.75, na.rm = T),
            lwr    = quantile(d_n, 0.25, na.rm = T)) %>% 
  #plot!
  ggplot() + 
    geom_hline(aes(yintercept=0), lty=2,lwd=1.25, col="grey30")+
    geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                  width = 0, 
                  col='grey30') + 
    geom_point(aes(x = transect, y = median), 
               pch=21,
               cex=4,
               col = 'grey30', 
               fill = '#33a02c',
               alpha = 70) + 
    theme_bw() + 
      ylab("Median Water Level [m]") + 
      xlab(NULL) + 
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) 

#Duration plot
dur<-metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect) %>% 
  summarise(median = median(dur_day, na.rm=T), 
            upr    = quantile(dur_day, 0.75, na.rm = T),
            lwr    = quantile(dur_day, 0.25, na.rm = T)) %>% 
  #plot!
  ggplot() + 
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = median), 
             pch=21,
             cex=4,
             col = 'grey30', 
             fill = '#ff7f00',
             alpha = 70) + 
  theme_bw() +
  ylab("Saturation Duration [Days]") + 
  xlab(NULL) + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

#Frequency plot
freq<-metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect) %>% 
  summarise(median = median(n_events, na.rm=T), 
            upr    = quantile(n_events, 0.75, na.rm = T),
            lwr    = quantile(n_events, 0.25, na.rm = T)) %>% 
  #plot!
  ggplot() + 
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = median), 
             pch=21,
             cex=4,
             col = 'grey30', 
             fill = '#6a3d9a',
             alpha = 70) + 
  theme_bw() + 
  ylab("Saturation Frequency [Events]") + 
  xlab(NULL) + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

#Print plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png("docs/hydro_regime.png", res=300, width = 7, height = 5.5, units = 'in')
hyd + dep + dur + freq + plot_layout(ncol=2)
dev.off()
