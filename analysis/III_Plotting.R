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

#Remove SC-A because it was inundaded and not samples
depth<-depth %>% filter(!(station == 'SC-A' & wetland == 'QB'))
metrics<-metrics %>% filter(!(station == 'SC-A' & wetland == 'QB'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Hydrologic Regime Plots-------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Hydrograph---------------------------------------------
df<-depth %>% 
  #Convert to cm
  mutate(d_n = d_n*100) %>% 
  #seleect wetland well data
  filter(str_detect(station, 'SC')) %>% 
  #create Hydrologic Zone col
  mutate(loc = substr(station, 4,4)) %>% 
  #Convert from depth to waterLevel
  mutate(waterLevel = -1*d_n) %>% 
  #Group by date & loc
  group_by(day, loc) %>% 
  #Summarise water level data
  summarise(
    mean = mean(waterLevel, na.rm = T),
    lwr    = mean - sd(waterLevel, na.rm = T)/sqrt(n()), 
    upr    = mean + sd(waterLevel, na.rm = T)/sqrt(n()))

#Subset by Hydrologic Zone
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
      ymin = -30, 
      ymax = 0), 
    fill='grey70', alpha = 0.9) +
  #D
  geom_ribbon(aes(ymin = d$lwr, ymax = d$upr, x = d$day, fill='D'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=d$day, y=d$mean), 
            col=line_col) +
  #E
  geom_ribbon(aes(ymin = e$lwr, ymax = e$upr, x = e$day, fill='E'), 
              col='grey90', lwd=0.25) +
  geom_line(aes(x=e$day, y=e$mean), 
            col=line_col) +
  
  #C
  geom_ribbon(aes(ymin = c$lwr, ymax = c$upr, x = c$day, fill='C'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=c$day, y=c$mean), 
            col=line_col) +
  #B
  geom_ribbon(aes(ymin = b$lwr, ymax = b$upr, x = b$day, fill="B"), 
              alpha=ribbon_alpha) +
  geom_line(aes(x=b$day, y=b$mean), 
            col=line_col) +
  #A
  geom_ribbon(aes(ymin = a$lwr, ymax = a$upr, x = a$day, fill='A'),
              alpha=ribbon_alpha) +
  geom_line(aes(x=a$day, y=a$mean), 
            col=line_col) +
  #Legend/color
  scale_fill_manual(name=NULL, values=cols) +
  #Clip to water year
  coord_cartesian(xlim=as.Date(c("2017-10-01", "2018-09-30"))) +
  #theme options
  theme_bw() + 
    ylab("Water Level [cm]") + 
    xlab(NULL) + 
    scale_x_date(date_labels = "%b") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = c("bottom"), 
          legend.margin = margin(t=-2, unit="lines")) +
    plot_annotation(tag_levels = 'A', tag_suffix = ".  ")


#2.2 Water Level Plot---------------------------------------
dep<-depth %>%
  #Convert depth-to-water-table to water level
  mutate(d_n = -1*d_n) %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect and station
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(d_n = mean(d_n, na.rm=T), na.rm=T) %>% 
  #Summarise by transect
  group_by(transect) %>% 
  summarise(mean = mean(d_n, na.rm=T)*100, 
            upr    = (mean(d_n, na.rm=T) + sd(d_n, na.rm = T)/sqrt(n()))*100,
            lwr    = (mean(d_n, na.rm=T) - sd(d_n, na.rm = T)/sqrt(n()))*100) %>% 
  #plot!
  ggplot() + 
    geom_hline(aes(yintercept=0), lty=2,lwd=1.25, col="grey30")+
    geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                  width = 0, 
                  col='grey30') + 
    geom_point(aes(x = transect, y = mean), 
               pch=c(21,22,23,24,25),
               cex=4,
               col = 'grey30', 
               fill = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00'), 
               alpha = 70) + 
    theme_bw() + 
      ylab("Water Level [cm]") + 
      xlab("Hydrologic Zone") + 
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) 

#2.3 Duration ----------------------------------------------
#Duration plot
dur<-metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(dur_day = mean(dur_day, na.rm=T), na.rm=T) %>% 
  group_by(transect) %>% 
  summarise(mean = mean(dur_day, na.rm=T), 
            upr    = mean(dur_day, na.rm=T) + sd(dur_day, na.rm=T)/sqrt(n()),
            lwr    = mean(dur_day, na.rm=T) - sd(dur_day, na.rm=T)/sqrt(n())) %>% 
  #plot!
  ggplot() + 
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = mean), 
             pch=c(21,22,23,24,25),
             cex=4,
             col = 'grey30', 
             fill = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00'),
             alpha = 70) + 
  theme_bw() +
  ylab("Saturation Duration [Days]") + 
  xlab('Hydrologic Zone') + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

#2.4 Frequency----------------------------------------------
freq<-metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(n_events = mean(n_events, na.rm=T), na.rm=T) %>% 
  group_by(transect) %>% 
  summarise(mean = mean(n_events, na.rm=T), 
            upr    = mean(n_events, na.rm=T)+sd(n_events, na.rm=T)/sqrt(n()),
            lwr    = mean(n_events, na.rm=T)-sd(n_events, na.rm=T)/sqrt(n())) %>% 
  #plot!
  ggplot() + 
  geom_errorbar(aes(x = transect, ymin = lwr, ymax = upr), 
                width = 0, 
                col='grey30') + 
  geom_point(aes(x = transect, y = mean), 
             pch=c(21,22,23,24,25),
             cex=4,
             col = 'grey30', 
             fill = c('#e41a1c', '#377eb8','#4daf4a','#984ea3','#ff7f00'),
             alpha = 70) + 
  theme_bw() + 
  ylab("Saturation Frequency [Events]") + 
  xlab("Hydrologic Zone") + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) 

#2.5 Print plot---------------------------------------------
tiff("docs/hydro_regime.tiff", res=285, width = 7, height = 6, units = 'in')
hyd + dep + dur + freq + plot_layout(ncol=2)
dev.off()

pdf("docs/hydro_regime.pdf", width = 7, height = 6)
hyd + dep + dur + freq + plot_layout(ncol=2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.6 Intext calculations--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
depth %>%
  #Convert depth-to-water-table to water level
  mutate(d_n = -1*d_n) %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect and station
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(d_n = mean(d_n, na.rm=T), na.rm=T) %>% 
  #Summarise by transect
  group_by(transect) %>% 
  summarise(mean_cm = mean(d_n, na.rm=T)*100, 
            sem_cm  = sd(d_n, na.rm = T)*100/sqrt(n()))

metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(dur_day = mean(dur_day, na.rm=T), na.rm=T) %>% 
  group_by(transect) %>% 
  summarise(mean_day = mean(dur_day, na.rm=T), 
            sem_day  = sd(dur_day, na.rm=T)/sqrt(n()))


metrics %>% 
  #Select SC stations
  filter(str_detect(station, 'SC')) %>% 
  #Summarise duration by transect
  mutate(transect = substr(station,4,4)) %>% 
  group_by(transect, wetland) %>%
  summarise(n_events = mean(n_events, na.rm=T), na.rm=T) %>% 
  group_by(transect) %>% 
  summarise(mean = mean(n_events, na.rm=T), 
            sem  = sd(n_events, na.rm=T)/sqrt(n()))

