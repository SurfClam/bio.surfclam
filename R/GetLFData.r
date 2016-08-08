#' @export
GetLFData <- function(update=T){
  ##############################################################################
  # Get data from database, download whole view "COM_LEN_FREQ'
  ##############################################################################
  if(update==T){
   # open DB connection  .. need to be defined elsewhere .. private file
   RODBCconn <- MakeConnection( ) 
   lf.q <- "Select 
       DECODE(SIGN(LATITUDE_DD - (39.0 + (ABS(LONGITUDE_DD)-50.0)*0.88333333)),-1,4,0,4,1,3) AREA,
       ROUND(M.LENGTH,0) RLENGTH,
       M.NUMBER_AT_LENGTH,
       M.ITIS_CODE,   M.SAMPLETYPE,  M.COMSAMPLEPROFILE_ID,
       P.LOGTRIP_ID, P.SAMPLE_DATE, P.SAMPLE_TIME,  P.LONGITUDE_DD,  P.LATITUDE_DD, P.DEPTH
   From CLAMCOMLENFREQ M, CLAMCOMSAMPLEPROFILE P
   Where M.ITIS_CODE = 80983
   And M.SAMPLETYPE = 'UNSORTED'
   And M.COMSAMPLEPROFILE_ID = P.COMSAMPLEPROFILE_ID(+)"
   lf.data <- sqlQuery(RODBCconn, lf.q)
   lf.data$YEAR <- year(lf.data$SAMPLE_DATE)
   names(lf.data) <- tolower(names(lf.data))
   odbcClose(RODBCconn)

    save(lf.data,file=file.path( project.datadirectory("bio.surfclam"), "data", "LFdata.Rdata" ))
  }
  else {
  	load(file.path( project.datadirectory("bio.surfclam"), "data", "LFdata.Rdata" ))
    names(lf.data) <- tolower(names(lf.data))

  }
 return(lf.data)
}

