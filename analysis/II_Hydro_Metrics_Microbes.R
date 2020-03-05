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
#plotting libraries
library(xts)
library(dygraphs)
#database libraries
library(devtools)
devtools::install_github("khondula/rodm2")
library(RSQLite)
library(DBI)
library(rodm2)
#Data wrangling
library(lubridate)
library(readxl)
library(tidyverse)

#Define working dir
working_dir<-"/nfs/palmer-group-data/Choptank/Nate/DepthToWaterTable/data/"

#Set system time zone 
Sys.setenv(TZ="America/New_York")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download data-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define database connection
db<-dbConnect(RSQLite::SQLite(), paste0(working_dir, "choptank.sqlite"))

#Define site names of interest
db_get_sites(db)








