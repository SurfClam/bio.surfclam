GetVMSData <- function(update=T){
  ##############################################################################
  # Get data from database
  ##############################################################################
  if(update==T){
  # open DB connection  .. need to be defined elsewhere .. private file
  RODBCconn <- MakeConnection( ) 
#####   RODBCconn <- odbcConnect("PTRAN", uid=oracle.personal.user, pwd=oracle.personal.password)

  #Define a list of VRNs from Clam database
  ves.q <- "SELECT DISTINCT CLAM.CLAMVESSEL.CFV
  FROM CLAM.CLAMVESSEL
  ORDER BY CLAM.CLAMVESSEL.CFV"
   
  vrn.list <- sqlQuery(RODBCconn, ves.q, believeNRows=FALSE)  
  vrn.list <- na.omit(vrn.list)
  vrn.list <- as.character(as.matrix(vrn.list))
  vrn.list <- paste(vrn.list,collapse="','")
   
  ##############	 
  # Select VMS #	
  ##############
   
 # vms.q <- paste("SELECT rownum vesid,
 #                 p.lon, p.lat, 
 #                NVL(v.vessel_name,p.vrn) vessel_name, 
 #                p.vrn, 'V_'||p.vrn vr_number,
 #                to_char(p.pdate, 'YYYY/MM/DD HH24:MI:SS') vmsdate,
 #                p.hailout,
 #                p.speed_knots 
 #                FROM mfd_obfmi.vms_pos p, mfd_obfmi.marfis_vessels_syn v
 #                WHERE p.vrn = v.vr_number(+) 
 #                AND p.vrn IN ('",vrn.list,"')",
 #                 sep=""
 # ) 
vms.q <- paste("SELECT rownum vesid,
                  p.longitude lon, p.latitude lat, 
                 NVL(v.vessel_name,p.vr_number) vessel_name, 
                 p.vr_number vrn,
                 to_char(p.POSITION_UTC_DATE, 'YYYY/MM/DD HH24:MI:SS') vmsdate,
                 p.speed_knots 
                 FROM mfd_obfmi.vms_all p, mfd_obfmi.marfis_vessels_syn v
                 WHERE p.VR_NUMBER = v.vr_number(+)  
                 AND p.vr_number IN ('",vrn.list,"')",
                  sep=""
  )

  vms.data <- sqlQuery(RODBCconn, vms.q, believeNRows=FALSE)  
  odbcClose(RODBCconn)

  save(vms.data,file=file.path( project.datadirectory("offshoreclams"), "data", "VMSdata.Rdata" ))
  }
  else {
  	load(file.path( project.datadirectory("offshoreclams"), "data", "VMSdata.Rdata" ))
  }
 return(vms.data)
}

