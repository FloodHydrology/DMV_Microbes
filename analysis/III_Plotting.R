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
depth<-read_csv("data//DepthToWaterTable.csv")
metrics<-read_csv("data//annual_metrics.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Hydrologic Regime Plots-------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Hydrograph---------------------------------------------
df<-depth %>% 
  #seleect wetland well data
  filter(str_detect(station, 'SC')) %>% 
  #create sampling location col
  mutate(loc = substr(station, 4,4)) %>% 
  #Convert from depth to waterLevel
  mutate(waterLevel = -1*d_n) %>% 
  #Group by date & loc
  group_by(day, loc) %>% 
  #Summarise water level data
  summarise(
    median = median(waterLevel, na.rm = T),
    lwr    = quantile(waterLevel, 0.25, na.rm = T), 
    upr    = quantile(waterLevel, 0.75, na.rm = T))

#Subset by sampling location
a<-df %>% filter(loc=='A')
b<-df %>% filter(loc=='B')
c<-df %>% filter(loc=='C')
d<-df %>% filter(loc=='D')
e<-df %>% filter(loc=="E")

#Define ribbon tranparency
ribbon_alpha<-0.90

#Define colors
cols<-c(
  'A' = '#045a8d', 
  'B' = '#2b8cbe', 
  'C' = '#74a9cf', 
  'D' = '#bdc9e1',
  'E' = '#f1eef6')
line_col<-"grey50"
  
#Start ggplot
hyd<-ggplot() + 
  geom_rect(
    aes(
      xmin = as.Date('2017-09-01'), 
      xmax = as.Date('2018-10-31'), 
      ymin = -0.30, 
      ymax = 0), 
    fill='grey70', alpha = 0.9) +
  #D
  geom_ribbon(aes(ymin = d$lwr, ymax = d$upr, x = d$day, fill='D'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=d$day, y=d$median), 
            col=line_col) +
  #E
  geom_ribbon(aes(ymin = e$lwr, ymax = e$upr, x = e$day, fill='E'), 
              col='grey90', lwd=0.25) +
  geom_line(aes(x=e$day, y=e$median), 
            col=line_col) +
  
  #C
  geom_ribbon(aes(ymin = c$lwr, ymax = c$upr, x = c$day, fill='C'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=c$day, y=c$median), 
            col=line_col) +
  #B
  geom_ribbon(aes(ymin = b$lwr, ymax = b$upr, x = b$day, fill="B"), 
              alpha=ribbon_alpha) +
  geom_line(aes(x=b$day, y=b$median), 
            col=line_col) +
  #A
  geom_ribbon(aes(ymin = a$lwr, ymax = a$upr, x = a$day, fill='A'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=a$day, y=a$median), 
            col=line_col) +
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  #Clip to water year
  coord_cartesian(xlim=as.Date(c("2017-10-01", "2018-09-30"))) +
  #theme options
  theme_bw() + 
    ylab("Water Level [m]") + 
    xlab(NULL) + 
    scale_x_date(date_labels = "%b") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = c("bottom"), 
          legend.margin = margin(t=-2, unit="lines"))


#2.2 Water Level Plot---------------------------------------
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
      xlab("Sampling Location") + 
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) 

#2.3 Duration ----------------------------------------------
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
  xlab('Sampling Location') + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

#2.4 Frequency----------------------------------------------
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
  xlab("Sampling Location") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

#2.5 Print plot---------------------------------------------
png("docs/hydro_regime.png", res=300, width = 7, height = 6, units = 'in')
hyd + dep + dur + freq + plot_layout(ncol=2)
dev.off()
