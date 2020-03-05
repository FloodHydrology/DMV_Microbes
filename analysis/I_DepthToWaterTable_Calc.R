#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Depth to water table calclulation
#Coder: C. Nate Jones (cnjones@umd.edu)
#Date: 3/5/2020
#Purpose: Estimate depth to water table at wetland sampling locations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1.0 Setup workspace------------------------------------------------------------
#Clear workspace
remove(list=ls())

#download relevant libraries
library(lubridate)
library(tidyverse)

#download data
df<-read_csv("data/waterLevel.csv")
survey<-read_csv("data/xs_survey.csv")

#2.0 Data Cleaning and gap filling----------------------------------------------
#2.1 BB Upland Well-------------------------------------------------------------
#Gather data
temp<-df %>%
  #Select time series of interest
  filter(site == 'BB Upland Well 1' |
           site == "TB Upland Well 3") %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(upland = 'BB Upland Well 1',
         fill   = "TB Upland Well 3") 

#Develop model
model<-lm(upland ~ fill, data=temp)
summary(model)

#Add model predictions and organize for output
temp<-temp %>%
  #Apply model
  mutate(predicted = predict(model, data.frame(fill = fill))) %>%
  #Gap fill
  mutate(waterLevel = if_else(is.na(upland), 
                              predicted, 
                              upland), 
         site = 'BB Upland Well 1') %>%
  select(day, site, waterLevel)

#Splice into df
df<-df %>% 
  filter(site != "BB Upland Well 1") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#2.2 DB Upland Well-------------------------------------------------------------
#Select water level data
#Gather data
temp<-df %>%
  #Select time series of interest
  filter(site == 'DB Upland Well 1' |
           site == "TB Upland Well 2") %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(upland = 'DB Upland Well 1',
         fill   = "TB Upland Well 2") 

#Develop model
model<-lm(upland ~ fill, data=temp)
summary(model)

#Add model predictions and organize for output
temp<-temp %>%
  #Apply model
  mutate(predicted = predict(model, data.frame(fill = fill))) %>%
  #Gap fill
  mutate(waterLevel = if_else(is.na(upland), 
                              predicted, 
                              upland), 
         site = 'DB Upland Well 1') %>%
  select(day, site, waterLevel)

#Splice into df
df<-df %>% 
  filter(site != 'DB Upland Well 1') %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#2.3 TB wetland well------------------------------------------------------------
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

#2.4 QB wetland well------------------------------------------------------------
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
                                       y_sw_2-0.09, #Based on survey
                                       y_sw_1), 
                      site = "QB Wetland Well Shallow") %>%
  select(day, site, y_sw_1) %>% rename(waterLevel = y_sw_1)

#Splice into df
df<-df %>% 
  filter(site != "QB Wetland Well Shallow") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#2.5 GN wetland well------------------------------------------------------------
#organize time series for model
temp<-df %>% 
  filter(site == "GN Wetland Well Shallow" |
         site == "GR Wetland Well Shallow") %>% 
  spread(site, -day) %>% 
  rename(nat = "GN Wetland Well Shallow",
         res = "GR Wetland Well Shallow")
  
#Create model 
model<-lm(nat ~ poly(res,5), data=temp)

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
         site = "GN Wetland Well Shallow") %>%
  #Clean up 
  select(day, site, waterLevel)

#Splice temp into df
df<-df %>% 
  filter(site != "GN Wetland Well Shallow") %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#2.6 GN Upland well-------------------------------------------------------------
#Gather data
temp<-df %>%
  #Select time series of interest
  filter(site == 'GN Upland Well 1' |
         site == "ND Upland Well 1") %>%
  #Create wide dataframe
  spread(site, -day) %>% 
  rename(upland = 'GN Upland Well 1',
         deep = "ND Upland Well 1") 

#Develop model
model<-lm(upland ~ poly(deep,1), data=temp)

#Add model predictions and organize for output
temp<-temp %>%
  #Apply model
  mutate(predicted = predict(model, data.frame(deep = deep))) %>%
  #Gap fill
  mutate(waterLevel = if_else(is.na(upland), 
                             predicted, 
                             upland), 
         site = 'GN Upland Well 1') %>%
  select(day, site, waterLevel)

#Splice temp into df
df<-df %>% 
  filter(site != 'GN Upland Well 1') %>%
  bind_rows(.,temp)

#Clean up temp files
remove(temp)

#2.7 Export daily water level---------------------------------------------------
write.csv(df, "data/waterLevel_cleaned.csv")

#3.0 Estimate Deth to Water Table-----------------------------------------------
#3.1 Depth to water table function----------------------------------------------
depth_fun<-function(wetland_code){
  
  #Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
  #Estimate depth to water table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  
  #Add wetland code
  output$wetland<-wetland_code
  
  #Export output
  output
}

#3.2 Apply Functions------------------------------------------------------------
sites<-c("BB", "DB", "DK", "GN", "ND","QB", "TB")
df<-lapply(sites, depth_fun) %>% bind_rows(.)

#3.3 Export file----------------------------------------------------------------
write.csv(df, "data/DepthToWaterTable.csv")

