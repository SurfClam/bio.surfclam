#' @export
assignLogData2VMS = function(fisheryList, p){

  b=ifelse(p$bank=="Ban",1,2)
  logdata = merge(fisheryList$vms.data,subset(fisheryList$log.data, bank==b&area>p$effort.threshold[1]&area<p$effort.threshold[2]&round_catch>p$catch.threshold[1]&round_catch<p$catch.threshold[2],  c("logrecord_id","round_catch","area")))
  vmsperwatch=with(logdata,tapply(logrecord_id,logrecord_id,length))
  logdata = merge(logdata,data.frame(logrecord_id=as.numeric(names(vmsperwatch)),vmspw=vmsperwatch),all=T)
  
  logdata$EID = 1:nrow(logdata)
  logdata$X = logdata$lon
  logdata$Y = logdata$lat
  logdata$A = logdata$area/logdata$vmspw
  logdata$C = logdata$round_catch/logdata$vmspw

  logdata = logdata[,c("EID","X","Y","C","A","year","vmsdatelocal")]

  save(logdata,file=file.path( project.datadirectory("bio.surfclam"), "data", "VMSlogData.Rdata" ))


  return(logdata)
}