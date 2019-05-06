#Create db_get_ts_function
db_get_ts<-function(db, site_code, variable_code_CV, start_datetime, end_datetime){
  
  #Check if db is compatable
  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, Nate wrote this.  Sooooo, only sqlite database connections are supported for this function until Kelly swoops in to save the day.")}
  
  #SQLite Database 
  if(class(db) == "SQLiteConnection"){
    
    #Retreive Sampling Feature ID for site code
    SamplingFeatureID<-RSQLite::dbGetQuery(db,
                                           "SELECT SamplingFeatureID 
                                              FROM SamplingFeatures 
                                              WHERE SamplingFeatureCode = :x", 
                                           params=list(x=site_code))
    
    #Retreive Feature Action ID[s] for site code
    FeatureActionID<-RSQLite::dbGetQuery(db,
                                         "SELECT FeatureActionID FROM FeatureActions WHERE SamplingFeatureID = :x", 
                                         params=list(x=SamplingFeatureID[,1]))
    
    #Retreive variable ID
    VariableID<-RSQLite::dbGetQuery(db, 
                                    "SELECT VariableID
                                      FROM Variables
                                      WHERE VariableNameCV = :x",
                                    params=list(x=variable_code_CV))
    VariableID<-data.frame(VariableID=rep(paste(VariableID),nrow(FeatureActionID)))
    
    #Retreive Result ID[s] for each feature action
    ResultID<-RSQLite::dbGetQuery(db,
                                  "SELECT ResultID 
                                      FROM Results 
                                      WHERE FeatureActionID = :x
                                        AND VariableID =:y", 
                                  params=list(x=FeatureActionID[,1],
                                              y=VariableID[,1]))
    
    #Retreive Result values
    Values<-RSQLite::dbGetQuery(db,
                                "SELECT ValueDateTime, DataValue
                                        FROM TimeSeriesResultValues
                                        WHERE ResultID = :x", 
                                params=list(x=ResultID[,1]))
    
    #Turn date into POSIXct 
    Values$ValueDateTime<-as.POSIXct(Values$ValueDateTime)
    
    #truncte to specified datetime window [if given]
    if(exists("start_datetime")==TRUE){
      #turn start and end dates into POSIXct format
      start_datetime <- as.POSIXct(start_datetime)
      end_datetime   <- as.POSIXct(end_datetime)
      
      #limit values df to defined window
      Values <- Values %>%
        filter(ValueDateTime >= start_datetime, 
               ValueDateTime <= end_datetime)
    }
  }
  
  #Clean up values df
  colnames(Values)<-c("Timestamp", variable_code_CV)
  
  #Export Values
  Values %>% arrange(Timestamp)
}