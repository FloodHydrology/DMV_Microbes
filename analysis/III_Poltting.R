#Script Information---------------------------------------------------------------------------------
#Title: Example analysis
#Coder: C. Nate Jones (cnjones@umd.edu)
#Date: 5/9/2019
#Purpose: Create depth to water table plots

# 1.0 Setup Workspace-------------------------------------------------------------------------------
#Clear memory 
remove(list=ls())

#load appropriate packages
library(gridExtra)
library(lubridate)
library(tidyverse)

#load data
level<-read_csv("data/waterLevel_cleaned.csv")
depth<-read_csv("data/DepthToWaterTable.csv")

#2.0 Barplot of mean depth to water table-----------------------------------------------------------
bar_plot<-depth %>%
  #Filter to site [SC for Kelly's and AIK for Anna's]
  filter(str_detect(station,"SC")) %>%
  #Convert depth to cm
  mutate(d_n = d_n*100) %>%
  #Estimate mean annual depths
  group_by(station, wetland) %>% summarise(Depth= mean(d_n, na.rm=T)) %>%
  #Plot
  ggplot(aes(x=station, y=Depth)) +
    #Add barplot
    geom_bar(stat="identity") + 
    #Add line for zero
    geom_hline(yintercept = 0, col="grey20") +
    #Facet
    facet_grid(rows=vars(wetland)) +
    #Theme information
    theme_bw() + 
      labs(y="Mean Depth to Water Table [cm]", 
           x= "Sampling Location")

#Time series plot of water level-------------------------------------------------------------------
#Tidy water level data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
level<-level %>%
  #Remove Auxilery Upland Wells
  filter(!str_detect(site,'2'), 
         !str_detect(site,'3'), 
         !str_detect(site,'DF'), 
         !str_detect(site,'GR')) %>%
  #Define wetland and well type
  mutate(well = if_else(str_detect(site,"Wetland"), 
                        'wetland', 
                        'upland'),
         site = substr(site,1,2)) %>%
  #Select relevant collumns
  select(site, well, day, waterLevel)

#Plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ts_plot<-level %>% 
  mutate(waterLevel=waterLevel*100) %>%
  filter(site!="GN") %>%
  ggplot(aes(day, waterLevel, color = well, linetype=well)) +
    geom_line() +
    facet_grid(rows=vars(site))+
    theme_bw() +
      labs(y="Mean Water Level [cm]", 
           x= "Date")+
      scale_linetype_manual(values=c(1,1)) +
      scale_color_manual(values=c('grey70','grey30')) + 
      scale_x_date(date_labels =  "%b")

#3.0 Print plots------------------------------------------------------------------------------------
output<-grid.arrange(bar_plot,ts_plot,nrow=1,widths = c(1,1.3))
ggsave(file = "SC_Depth_waterLevel.png", plot=output, path = "results/",
        device = "png", width = 7, height = 8, units = "in")
