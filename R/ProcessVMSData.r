#' @export

ProcessVMSData <- function(vms.data,log.data){
  
  ##############################################################################
  ## Do initial processing, fill in some missing values, 
  ##############################################################################

  names(vms.data) <- tolower(names(vms.data))
  vms.data$vmsdate <- as.POSIXct(vms.data$vmsdate,tz="GMT")  # VMS data is in UTC, assign timezone
  vms.data$vessel_name <- tolower(vms.data$vessel_name)
  ########################################
  # Clean VMS data selected from VMS_pos #
  ########################################
  
  # Shift vmsdate with a duplicate within vrn by one second (duplicate record is moved one second forward)
  #vms.data$vmsdate <- adjust.duplicateTimes(vms.data$vmsdate, vms.data$vrn)
  # Create date and time variables in local time
   vms.data$vmsdatelocal <- format(vms.data$vmsdate,format="%Y-%m-%d %H:%M:%S", tz="America/St_Johns",usetz=TRUE)
  vms.data$date <- as.character(strptime(vms.data$vmsdatelocal,format="%Y-%m-%d"))
  vms.data$year <- year(vms.data$vmsdate)
  #vms.data$time <- as.POSIXct(vms.data$time,format="%H:%M:%S")

  # Add Watch number (record_no) to VMS data from local Date:Time 
  watchTimes = c("06:00:00","12:00:00","18:00:00")
  vms.data$record_no <- 0
  vms.data$record_no[vms.data$vmsdatelocal<as.POSIXct(paste(vms.data$date,watchTimes[1]))] <- 1
  vms.data$record_no[vms.data$vmsdatelocal>=as.POSIXct(paste(vms.data$date,watchTimes[1]))&vms.data$vmsdatelocal<as.POSIXct(paste(vms.data$date,watchTimes[2]))] <- 2
  vms.data$record_no[vms.data$vmsdatelocal>=as.POSIXct(paste(vms.data$date,watchTimes[2]))&vms.data$vmsdatelocal<as.POSIXct(paste(vms.data$date,watchTimes[3]))] <- 3
  vms.data$record_no[vms.data$vmsdatelocal>=as.POSIXct(paste(vms.data$date,watchTimes[3]))] <- 4
  #vms.data <- vms.data[!duplicated(vms.data),] # Removes any rows that are fully duplicated
  
  ########################################	
  # Cross against logs to pull out trips #
  ########################################	
  
  # Assign logrecord_id to vms.data
  processed.vms.data1 <- merge(vms.data,subset(log.data,year>1999&area>0,c("logrecord_id","cfv","date","record_no","vessel_name")), by.x = c("vrn", "date", "record_no"), by.y = c("cfv","date","record_no"))#,all.x=TRUE) 
  #processed.vms.data2 <- merge(vms.data,subset(log.data,year>1999,c("logrecord_id","cfv","date","record_no","vessel_name")), by.x = c("vrn", "date", "record_no"), by.y = c("cfv","date","record_no"),all.y=TRUE) 
  
  # Order dataframe by vrn and DateTime 
  processed.vms.data1 <- processed.vms.data1[order(processed.vms.data1$vrn, processed.vms.data1$vmsdatelocal), ]  # Order dataframe by vrn and DateTime 

#browser()
   # get VMS postion for log data
   vmsPos <- aggregate(cbind(lon,lat) ~ logrecord_id, data = processed.vms.data1, median)
   log.data <- merge(log.data,vmsPos,all=T)


  # checking for when we have VMS but no log
  #log.data$vesday=paste(log.data$cfv,log.data$date,sep='.')
  #processed.vms.data1$vesday=paste(processed.vms.data1$vrn,processed.vms.data1$date,sep='.')
  #nologs=unique(subset(processed.vms.data1,is.na(logrecord_id))$vesday)
  #subset(log.data,vesday%in%nologs)

  # Check for outliers in latitude and longitude  
  
  # Calculate distance travelled between points
  
  # Remove watches without effort
  #Note we lose catch data by removing watches without effort since there is a delay
  #log.data <- log.data[log.data$n_tows!=0,]
  
  return(list(vms.data=processed.vms.data1,log.data=log.data))

} # end of function ProcessLogData