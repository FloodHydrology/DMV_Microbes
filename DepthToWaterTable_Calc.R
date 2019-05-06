####################################################################################################
#Title: Depth to water table calclulation
#Coder: C. Nate Jones (cnjones@umd.edu)
#Date: 5/4/2019
#Purpose: Estimate depth to water table at wetland sampling locations
####################################################################################################

#1.0 Setup workspace--------------------------------------------------------------------------------
#Clear workspace
remove(list=ls())

#download relevant libraries
devtools::install_github("khondula/rodm2")
library(rodm2)
library(RSQLite)
library(DBI)
source('db_get_ts.R')
library(lubridate)
library(tidyverse)

#Set relevant directories
working_dir<-"/nfs/palmer-group-data/Choptank/Nate/DepthToWaterTable/data/"

#Download relevant data
survey<-read.csv(paste0(working_dir, 'survey.csv')) %>%
  mutate(distance = distance_ft*.3048, 
         elevation = elevation_ft*.3048) %>%
  select(wetland, distance, elevation, station)

#2.0 Organize wetland water level data-------------------------------------------------------------
#Connect to database
db<-dbConnect(RSQLite::SQLite(),paste0(working_dir,"choptank.sqlite"))

#Define start and end date for downloads
start_date<-as.POSIXct(mdy("10-1-2017"))
end_date<-as.POSIXct(mdy("9-30-2018"))

#Define download sites
wells<-c(
  #Bubbly Bay
  "BB Upland Well 1", "BB Wetland Well Shallow",
  #Dank Bay
  "DB Upland Well 1", "DB Wetland Well Shallow",
  #Dark Bay Wetland Well Shallow
  "DK Upland Well 1", "DK Wetland Well Shallow",
  #Greg Nat Wells
  # "GN Upland Well 1", "GN Wetland Well Shallow", "GR Wetland Well Shallow",
  #North Dogbone Wells
  "ND Upland Well 1", "ND Wetland Well Shallow",
  #Quintessential Bay
  "QB Upland Well 1", "QB Wetland Well Shallow", "DF Wetland Well Shallow", 
  #Tiger Paw -- B
  "TB Upland Well 1","TB Upland Well 2","TB Upland Well 3", "TB Wetland Well Shallow") 
  
#Create function to download water level data
fun<-function(site){
  #download data
  temp<-db_get_ts(db, site, "waterLevel", start_date, end_date)
  
  #add site collumn
  temp$site = site
  
  #Export to .GlovalEnv
  temp 
}

#Apply download function
df<-lapply(wells, fun) %>% bind_rows(.)

#Estimate mean daily water level for each site
df<-df %>%
  #truncate to date
  mutate(day = floor_date(Timestamp, "day")) %>%
  #group by date and site
  select(day, waterLevel, site) %>%
  group_by(day, site) %>%
  summarise(waterLevel = mean(waterLevel, na.rm=T))


#3.0 Gap filling------------------------------------------------------------------------------------
#Do this later


#4.0 Estimate Deth to Water Table-------------------------------------------------------------------
#Create to estimate depth to water table at each location for each timestep
depth_fun<-function(wetland_code){
  
  #Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create wide df of wetland and upland water level data
  temp<-df %>%
    #Select time series of interest
    filter(site == paste0(wetland_code, ' Wetland Well Shallow') |
           site == paste0(wetland_code, ' Upland Well 1')) %>%
    #Create wide dataframe
    spread(site, -day) %>% 
    rename(y_sw = paste0(wetland_code, ' Wetland Well Shallow'),
           y_gw = paste0(wetland_code, ' Upland Well 1'))
  
  #Cross section data
  xs<-survey %>% filter(wetland == wetland_code) %>% select(distance, elevation)

  #Define points of interest
  sample<- survey %>% 
    filter(wetland == wetland_code) %>% 
    filter(str_detect(station,"AIK") |
           str_detect(station, 'SC')) %>% 
    select(station, distance, elevation)
  
  #Define Wells [for now, assume wetladn well x = 0]
  well<- survey %>% 
    filter(wetland == wetland_code) %>% 
    filter(str_detect(station,"Upland Well")) %>% 
    select(distance, elevation)
  
  #Estimate depth to water table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Start function to estimate depth to water table for each sampling point
  inner_fun<-function(n){
    
    #Define sample
    sample<-sample[n,]
    
    #Create interpolation to estimate inundation extent
    inundation_fun<-approxfun(xs$elevation, xs$distance)
    
    #Define horizontal distances 
    temp <- temp %>%
      mutate(
        #Distance from well to edge of water
        x_sw = inundation_fun(y_sw), 
        #distance from wetland to upland well
        x_gw = well$distance,
        #distance from wetland well to sampling location
        x_n = sample$distance)
    
    #Define water level at sample location
    temp <- temp %>%
      mutate(y_n = 
               #If sampling the sampling location is beyond upland well
               if_else(x_n>=x_gw, 
                       y_gw,
                  #If sampling location is inundated
                  if_else(x_n<=x_sw, 
                          y_sw, 
                    #If sampling location between inunation and upland well
                    (y_gw-y_sw)*(x_n-x_sw)/(x_gw-x_sw) + y_sw)))
    
    #Estimate depth
    temp<-temp %>% mutate(d_n = sample$elevation - y_n, 
                          station = sample$station)
    
    #Organize and export
    temp %>% select(day, d_n, station)
  }

  #Apply function to all sites                   
  output<-lapply(seq(1, nrow(sample)),inner_fun) %>% bind_rows(.)
  
  #Export output
  output
}

#Apply function to wetlands of interest
depth_fun("ND")

