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
  "GN Upland Well 1", "GN Wetland Well Shallow", "GR Wetland Well Shallow", "P-26 Deep Well",
  #North Dogbone Wells
  "ND Upland Well 1", "ND Wetland Well Shallow",
  #Quintessential Bay
  "QB Upland Well 1", "QB Wetland Well Shallow", "DF Wetland Well Shallow", 
  #Tiger Paw -- B
  "TB Upland Well 1","TB Upland Well 2","TB Upland Well 3", "TB Wetland Well Shallow") 

#Add var information
wells <- tibble(
  site = wells, 
  var = 'waterLevel'
)

#Define Exceptions
wells$var[wells$site == 'P-26 Deep Well'] <- 'waterDepth'
wells$var[wells$site ==  "GN Wetland Well Shallow"] <- 'waterDepth'
wells$var[wells$site ==  "GR Wetland Well Shallow"] <- 'waterDepth'

#Create function to download water level data
fun<-function(n){
  #download data
  temp<-db_get_ts(db, wells$site[n], wells$var[n], start_date, end_date)
  
  #add site collumn
  colnames(temp)<-c("Timestamp", "waterLevel")
  temp$site = wells$site[n]
  
  #Export to .GlovalEnv
  temp 
}

#Apply download function
df<-lapply(seq(1, nrow(wells)), fun) %>% bind_rows(.)

#Estimate mean daily water level for each site
df<-df %>%
  #truncate to date
  mutate(day = floor_date(Timestamp, "day")) %>%
  #group by date and site
  select(day, waterLevel, site) %>%
  group_by(day, site) %>%
  summarise(waterLevel = mean(waterLevel, na.rm=T))

#3.0 Gap filling------------------------------------------------------------------------------------
#3.1 BB wetland well--------------------------------------------------------------------------------
#Select water level data
temp<-df %>%
  #Select time series of interest
  filter(site == 'BB Wetland Well Shallow' |
         site == 'BB Upland Well 1') %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(y_sw = 'BB Wetland Well Shallow',
         y_gw = 'BB Upland Well 1')

#Convert NA in GW to SW values
temp<-temp %>% mutate(y_gw = if_else(is.na(y_gw),
                             y_sw, 
                             y_gw), 
                      site = "BB Upland Well 1") %>%
  select(day, site, y_gw) %>% rename(waterLevel = y_gw)

#Splice into df
df<-df %>% 
  filter(site != "BB Upland Well 1") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#3.2 DB wetland well--------------------------------------------------------------------------------
#Select water level data
temp<-df %>%
  #Select time series of interest
  filter(site == 'DB Wetland Well Shallow' |
           site == 'DB Upland Well 1') %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(y_sw = 'DB Wetland Well Shallow',
         y_gw = 'DB Upland Well 1')

#Convert NA in GW to SW values
temp<-temp %>% mutate(y_gw = if_else(is.na(y_gw),
                                     y_sw, 
                                     y_gw), 
                      site = "DB Upland Well 1") %>%
  select(day, site, y_gw) %>% rename(waterLevel = y_gw)

#Splice into df
df<-df %>% 
  filter(site != "DB Upland Well 1") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#3.3 TB wetland well--------------------------------------------------------------------------------
#Select water level data
temp<-df %>%
  #Select time series of interest
  filter(str_detect(site,"TB")) %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(y_sw = 'TB Wetland Well Shallow',
         y_gw_1 = 'TB Upland Well 1',
         y_gw_2 = 'TB Upland Well 2',
         y_gw_3 = 'TB Upland Well 3')

#Create model 
model<-lm(temp$y_gw_1~temp$y_gw_2)

#Convert NA in GW to SW values
temp<-temp %>% mutate(y_gw_1 = if_else(is.na(y_gw_1),
                                       model$coefficients[2]*y_gw_2+model$coefficients[1], 
                                       y_gw_1), 
                      site = "TB Upland Well 1") %>%
  select(day, site, y_gw_1) %>% rename(waterLevel = y_gw_1)

#Splice into df
df<-df %>% 
  filter(site != "TB Upland Well 1") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#3.4 QB wetland well--------------------------------------------------------------------------------
#Select water level data
temp<-df %>%
  #Select time series of interest
  filter(site == 'QB Wetland Well Shallow' |
         site == "DF Wetland Well Shallow") %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(y_sw_1 = 'QB Wetland Well Shallow',
         y_sw_2 = "DF Wetland Well Shallow")

#Convert NA in GW to SW values
temp<-temp %>% mutate(y_sw_1 = if_else(is.na(y_sw_1),
                                       y_sw_2-0.12, #BAsed on lining up early winter points
                                       y_sw_1), 
                      site = "QB Upland Well 1") %>%
  select(day, site, y_sw_1) %>% rename(waterLevel = y_sw_1)

#Splice into df
df<-df %>% 
  filter(site != "QB Upland Well 1") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#3.5 GN wetland well--------------------------------------------------------------------------------
#organize time series for model
temp_1<-db_get_ts(db, 
                  "GN Wetland Well Shallow", 
                  'waterDepth', ymd("1900-01-01"), ymd("3000-01-01")) %>%
  mutate(Timestamp = floor_date(Timestamp, "day")) %>%
  group_by(Timestamp) %>%
  summarise(waterDepth = mean(waterDepth, na.rm=T)) %>%
  rename(nat = "waterDepth")
temp_2<-db_get_ts(db, 
                  "GR Wetland Well Shallow", 
                  'waterDepth', ymd("1900-01-01"), ymd("3000-01-01")) %>%
  mutate(Timestamp = floor_date(Timestamp, "day")) %>%
  group_by(Timestamp) %>%
  summarise(waterDepth = mean(waterDepth, na.rm=T)) %>%
  rename(res = "waterDepth")
model<-left_join(temp_1, temp_2) %>% na.omit(); remove(temp_1);remove(temp_2)
model<-lm(nat ~ poly(res,5), data=model)

temp<-df %>%
  #Select time series of interest
  filter(site == 'GN Wetland Well Shallow' |
         site == "GR Wetland Well Shallow") %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(nat = 'GN Wetland Well Shallow',
         res = "GR Wetland Well Shallow") %>%
  #Apply model
  mutate(predicted = predict(model, data.frame(res = res))) %>%
  #Gap fill
  mutate(waterLevel = if_else(is.na(nat), 
                              predicted,
                              nat),
         site = "GR Wetland Well Shallow") %>%
  #Clean up 
  select(day, site, waterLevel)

#Splice temp into df
df<-df %>% 
  filter(site != "GR Wetland Well Shallow") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#3.5 GN Upland well--------------------------------------------------------------------------------


temp<-df %>%
  #Select time series of interest
  filter(site == 'GN Wetland Well Shallow' |
           site == "GR Wetland Well Shallow") %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(nat = 'GN Wetland Well Shallow',
         res = "GR Wetland Well Shallow") %>%
  #Apply model
  mutate(predicted = predict(model, data.frame(res = res))) %>%
  #Gap fill
  mutate(waterLevel = if_else(is.na(nat), 
                              predicted,
                              nat),
         site = "GR Wetland Well Shallow") %>%
  #Clean up 
  select(day, site, waterLevel)

#Splice temp into df
df<-df %>% 
  filter(site != "GR Wetland Well Shallow") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)



















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
    inundation_fun<-approxfun(xs$elevation, xs$distance, yleft = 0)
    
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
depth_fun('QB') %>%
  mutate(station = paste(station)) %>%
  filter(str_detect(station,"SC")) %>%
  group_by(station) %>% summarise(Depth= mean(d_n, na.rm=T)) %>%
  ggplot(aes(x=station, y=Depth)) +
    geom_bar(stat="identity") + theme_bw()

#Work in the AM -- check out why there are NA's in QB
