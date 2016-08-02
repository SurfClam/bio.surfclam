
ProcessLogData <- function(log.data){
  
  ##############################################################################
  ## Do initial processing, assign fishing areas, fill in some missing values, 
  ## set up coordinates for Banquereau and Grand Bank
  ##############################################################################
  
  data.names = names(log.data)[1:23]

  # dealing with duplicates
  x=log.data[duplicated(log.data[,c('VESSEL_NAME','RECORD_DATE','RECORD_NO')])|duplicated(log.data[,c('VESSEL_NAME','RECORD_DATE','RECORD_NO')],fromLast=T),]

  col1 <-c("LOGRECORD_ID", "VESSEL_NAME",  "CFV", "RECORD_DATE",  "RECORD_NO", "TRIP_NO",  "LOGTRIP_ID", "SUBTRIP_NO",  "LOGGEAR_ID","BLANCHED", "RAW_CLAMS", "WHOLE", "ROUND_CATCH")
  col2 <-c("CFV",  "RECORD_DATE",  "RECORD_NO", "LAT_DD", "LON_DD", "NAFO", "SPEED", "AVE_TIME", "N_TOWS",  "B_WIDTH", "AREA_TOWED")

  y=merge(subset(x,is.na(LAT_DD)&year(RECORD_DATE)>2010,col1),subset(x,!is.na(LAT_DD)&year(RECORD_DATE)>2010,col2),all=T)


  log.data = merge(log.data[!log.data$LOGRECORD_ID%in%x$LOGRECORD_ID,],y,all=T)[,data.names ]
  

  ##add zero to time if only three characters
  log.data$START_TIME[!is.na(log.data$START_TIME)] <- sprintf("%04d",log.data$START_TIME[!is.na(log.data$START_TIME)])
  log.data$START_TIME[!is.na(log.data$START_TIME)] <- gsub("(..)\\B", "\\1:", log.data$START_TIME[!is.na(log.data$START_TIME)])
  ##There are NAs for START_TIME, assign watch time for these records
  log.data$START_TIME[is.na(log.data$START_TIME)&log.data$RECORD_NO==1] <- "00:00"
  log.data$START_TIME[is.na(log.data$START_TIME)&log.data$RECORD_NO==2] <- "06:00"
  log.data$START_TIME[is.na(log.data$START_TIME)&log.data$RECORD_NO==3] <- "12:00"
  log.data$START_TIME[is.na(log.data$START_TIME)&log.data$RECORD_NO==4] <- "18:00"
  #
  ##Assuming all dates are NS time
  #log.data$record_date <- as.POSIXct(log.data$record_date,tz="America/Halifax")
  log.data$RECORD_DATE <- as.POSIXct(paste(log.data$RECORD_DATE, log.data$START_TIME), format="%Y-%m-%d %H:%M",tz="America/Halifax")
  ##create date and time variables
  log.data$date <- strftime(log.data$RECORD_DATE,format="%Y-%m-%d",tz="America/Halifax")
  log.data$time <- strftime(log.data$RECORD_DATE,format="%H:%M:%S",tz="America/Halifax")
  #log.data <- log.data[order(log.data$cfv, log.data$watch_date, log.data$logrecord_id),]  # Order dataframe by vrn, record_date/watch_date, and logrecord_id 
  
   
  ## Change NAs in N_TOWS and ROUND CATCH to 0s, usually valid 0's
  log.data$N_TOWS[is.na(log.data$N_TOWS)] <- 0  
  log.data$ROUND_CATCH[which(is.na(log.data$ROUND_CATCH))] <- 0  
  
  ##############################################################################
  ## Assign fishing area, currently Banquereau (in NAFO 4Vsc = 1), 
  ##                                Grand Bank (in 3L,3O or 3N = 2) or 
  ##                                Outside (=0)
  ## Consider trips that were actually early surveys covering Scotian Shelf
  ##############################################################################
  
  log.data$BANK <- rep(0,dim(log.data)[1])
  log.data$BANK[which(log.data$NAFO %in%  c("4VSC",'4VS'))] <- 1
  log.data$BANK[which(log.data$NAFO  %in% c("3L","3O","3N"))] <- 2
  
  ##############################################################################
  ## TOWING SPEED and TIME are fairly constant so for missing values fill 
  ## the average values for that vessel-trip-subtrip
  ##############################################################################
  
  t <- which((is.na(log.data$AVE_TIME) & log.data$N_TOWS > 0))
  if(length(t) > 0) {
    for(i in 1:length(t)) {
      recs <- which((log.data$CFV == log.data[t[i], "CFV"]) & 
                     (log.data$TRIP_NO == log.data[t[i], "TRIP_NO"]) & 
                     (log.data$SUBTRIP_NO == log.data[t[i], "SUBTRIP_NO"]))
      log.data$AVE_TIME[t[i]] <- mean(log.data$AVE_TIME[recs], na.rm = T)
    }
  }
  
  ## if any remaining fill in by CFV-trip
  t <- which((is.na(log.data$AVE_TIME) & log.data$N_TOWS > 0))
  if(length(t) > 0) {
    for(i in 1:length(t)) {
      recs <- which((log.data$CFV == log.data[t[i], "CFV"]) & 
                     (log.data$TRIP_NO == log.data[t[i], "TRIP_NO"]))
      log.data$AVE_TIME[t[i]] <- mean(log.data$AVE_TIME[recs], na.rm = T)
    }
  }
  
  ## Now do same for tow speed
  s <- which((is.na(log.data$SPEED) & log.data$N_TOWS > 0))
  if(length(s) > 0) {
    for(i in 1:length(s)) {
      recs <- which((log.data$CFV == log.data[s[i], "CFV"]) & 
                   (log.data$TRIP_NO == log.data[s[i], "TRIP_NO"]) & 
                   (log.data$SUBTRIP_NO == log.data[s[i], "SUBTRIP_NO"]))
      log.data$SPEED[s[i]] <- mean(log.data$SPEED[recs], na.rm = T)
    }
  }  
  
  ## if any remaining fill in by CFV-trip
  s <- which((is.na(log.data$SPEED) & log.data$N_TOWS > 0))
  if(length(s) > 0) {
    for(i in 1:length(s)) {
      recs <- which((log.data$CFV == log.data[s[i], "CFV"]) & 
                   (log.data$TRIP_NO == log.data[s[i], "TRIP_NO"]))
      log.data$SPEED[s[i]] <- mean(log.data$SPEED[recs], na.rm = T)
    }
  }
  
  ## Recalculate AREA with some NA's replaced in SPEED and AVE_TIME, 
  ## use this instead of original AREA_TOWED
  log.data$AREA <- (log.data$SPEED * 1000 * log.data$AVE_TIME * 
                      log.data$B_WIDTH * log.data$N_TOWS / 60.0)
  log.data$AREA[which(is.na(log.data$AREA))] = 0  ## change AREA NA's to 0,
  
  ## Dates: add time to dates for each watch (there is probably a prettier way to do this)
  #log.data$RECORD_DATE[log.data$RECORD_NO==2&as.numeric(format(log.data$RECORD_DATE,"%H"))==0] <- log.data$RECORD_DATE[log.data$RECORD_NO==2&as.numeric(format(log.data$RECORD_DATE,"%H"))==0]+6*60*60
  #log.data$RECORD_DATE[log.data$RECORD_NO==3&as.numeric(format(log.data$RECORD_DATE,"%H"))==0] <- log.data$RECORD_DATE[log.data$RECORD_NO==3&as.numeric(format(log.data$RECORD_DATE,"%H"))==0]+12*60*60
  #log.data$RECORD_DATE[log.data$RECORD_NO==4&as.numeric(format(log.data$RECORD_DATE,"%H"))==0] <- log.data$RECORD_DATE[log.data$RECORD_NO==4&as.numeric(format(log.data$RECORD_DATE,"%H"))==0]+18*60*60
  #log.data$RECORD_DATE[log.data$RECORD_NO==2&as.numeric(format(log.data$RECORD_DATE,"%H"))==1] <- log.data$RECORD_DATE[log.data$RECORD_NO==2&as.numeric(format(log.data$RECORD_DATE,"%H"))==1]+5*60*60
  #log.data$RECORD_DATE[log.data$RECORD_NO==3&as.numeric(format(log.data$RECORD_DATE,"%H"))==1] <- log.data$RECORD_DATE[log.data$RECORD_NO==3&as.numeric(format(log.data$RECORD_DATE,"%H"))==1]+11*60*60
  #log.data$RECORD_DATE[log.data$RECORD_NO==4&as.numeric(format(log.data$RECORD_DATE,"%H"))==1] <- log.data$RECORD_DATE[log.data$RECORD_NO==4&as.numeric(format(log.data$RECORD_DATE,"%H"))==1]+17*60*60

  # Errors
  log.data$LON_DD[log.data$LOGRECORD_ID==57984] = -59.44306
  log.data$LON_DD[log.data$LOGRECORD_ID==58040] = -59.43972
  log.data$LON_DD[log.data$LOGRECORD_ID==59011] = -58.96639
  log.data$LON_DD[log.data$LOGRECORD_ID==58943] = -58.34972
  log.data$LAT_DD[log.data$LOGRECORD_ID==66160] = 45.29833
  log.data$LAT_DD[log.data$LOGRECORD_ID==66620] = 45.60833
  log.data$LAT_DD[log.data$LOGRECORD_ID==63002] = 45.08306

  ## Set dome global parameters
  Min_lat <<- c(44.0, 43.0)        ## First set of values for Banquereau
  Max_lat <<- c(45.2, 46.5)       ## Second set for Grand Bank
  Min_long <<- c(-60.1, -51.5)
  Max_long <<- c(-57.1, -48.5)
  Area <<- c(10908.1, 49473.0)

  log.data <- log.data[order(log.data$RECORD_DATE),]
  names(log.data) <- tolower(names(log.data))

  return(log.data)

} # end of function ProcessLogData



