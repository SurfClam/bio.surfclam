###############################################################################
##
##  Artic Surf Clam Assessment Script
##
##  April 2017 
##
##  Brad Hubley
##  Susan Heaslip
##  Ryan Stanley
##
###############################################################################

#install_github("Beothuk/bio.base")
#require(bio.base)
#homedir = file.path( "C:" )  # mapping to root dir
#bio.workdirectory = file.path( homedir, "tmp" )      ### replace with correct path
#bio.directory = file.path( homedir, "bio" )   ### replace with correct path
#bio.datadirectory = file.path( homedir, "bio.data" )   ### replace with correct path


RLibrary( "devtools","PBSmapping", "lubridate", "trip", "fields","spatstat","TeachingDemos","gstat","CircStats","splancs","RODBC","RColorBrewer") # Load required packages

# install bio.packages from GitHub
#install_github("Beothuk/bio.base")
#install_github("Beothuk/bio.polygons")
#install_github("Beothuk/bio.utilities")
#install_github("LobsterScience/bio.lobster")
#install_github("BradHubley/SpatialHub")

# install bio.surfclam from local copy
#install_git("C:/bio/bio.surfclam")
#load_all("C:/bio/bio.surfclam")
#install_git("/home/hubleyb/bio/bio.surfclam")
  
	RLibrary("bio.surfclam","bio.lobster","bio.utilities","bio.polygons","SpatialHub")#,"bio.spacetime","bio.temperature")



## Load Data

update.data=F # TRUE accesses data from database if on a DFO windows machine


  # log data
  processed.log.data = ProcessLogData(GetLogData(update=update.data))
  #log.data = GetLogData(update=update.data)
  # save(processed.log.data,file=file.path( project.datadirectory("bio.surfclam"), "data", "processedLogdata.Rdata" ))

  #VMS data
  #vms.data = GetVMSData(update=update.data)
  fisheryList = ProcessVMSData(GetVMSData(update=update.data),processed.log.data)
  #processed.log.data = fisheryList$log.data
  processed.vms.data = fisheryList$vms.data
  #load(file=file.path( project.datadirectory("bio.surfclam"), "data", "griddedFisheryDataTotal.Rdata" ))

  # length frequency data
  lf.data = GetLFData(update=update.data)
  # save(lf.data,file=file.path( project.datadirectory("bio.surfclam"), "data", "lengthfreq.Rdata" ))
  


  # bounding polygon at 100m isobath for Banquereau
  c100 = read.table(file.path( project.datadirectory("bio.polygons"), "data","Basemaps","Marine","Bathymetry","CHS100.ll"),header=T)
  Banq100 = na.omit(subset(c100,SID==2392)) # 100m isobath for Banqureau

  # Clearwater Zones
  CWzones = read.csv(file.path( project.datadirectory("bio.surfclam"), "data","maps","CWzones.csv"))

  # New Areas
  new.areas  = read.csv(file.path( project.datadirectory("bio.surfclam"), "R","newareas.csv"))
  VMSden.poly = read.csv(file.path( project.datadirectory("bio.surfclam"),'data',"VMSpolygons.csv"))
 

## parameters

  p=list()
  p$bank= "Ban"
  p$yrs= list(2004:2016)
  #p$yrs= list(1986:2010,2000:2010,2009:2010)
  p$effort.threshold = c(15000,200000)
  p$catch.threshold = c(1500,30000)
  p$cpue.threshold = c(15000)
  p$record.levels = c(1,5,10,20,50,100,200,500)
  p$effort.levels = c(1000,50000,100000,200000,500000,1000000,2000000,5000000)
  p$catch.levels = c(100,5000,10000,20000,50000,100000,200000,500000)
  p$cpue.levels = c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.2)
  p$effort.cols = "YlGnBu"
  p$catch.cols = "YlGnBu"
  p$cpue.cols = "YlGnBu"
  p$Min_lon = -60.0
  p$Max_lon = -57.0
  p$Min_lat = 44.0
  p$Max_lat = 45.25
  p$grid.size = 1
  #p$grid.size = 1.852



####### Maps

  ClamMap2('all',isobath=seq(50,500,50))

  with(subset(processed.log.data,area>0),points(lon_dd,lat_dd,pch=16,cex=0.1,col=rgb(0,0,0,0.1)))
  with(subset(processed.log.data,year==2016&area>0),points(lon_dd,lat_dd,pch=16,cex=0.2,col=rgb(1,0,0,0.2)))
  with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))
  rect(Min_long,Min_lat,Max_long,Max_lat)


  ClamMap2('Ban',isobath=seq(50,500,50),title=i,bathy.source='bathy',nafo='all')
  with(subset(processed.log.data,year==2014&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,1,0,0.2)))
  with(subset(processed.log.data,year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
  with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,0,0,0.2)))


  ClamMap2('Grand',isobath=seq(50,500,50))

  rect(Min_long,Min_lat,Max_long,Max_lat)
  with(subset(processed.log.data,year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
  with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))

  ClamMap2('Ban',isobath=seq(50,500,50),bathcol='grey',bathy.source='bathy')
  addPolys(new.areas)
  addLabels(data.frame(PID=1:5,label=1:5),polys=new.areas,placement="CENTROID",cex=2,font=2)

  # VMS Data
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","FishingLocations.pdf"),11,8)
  for (i in 2002:2016) {
  ClamMap2('Ban',isobath=seq(50,500,50),title=i,bathy.source='bathy',nafo='all')
  with(subset(processed.log.data,year==i&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(1,0,0,0.2)))
  with(subset(processed.vms.data,year==i),points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  legend('bottomright',c("logs","vms"),pch=16,col=c(rgb(1,0,0,0.2),rgb(0,0,0,.1)),cex=c(0.5,0.2))
  }
  dev.off()

  # VMS GIF!!! takes awhile
  # version 1: daily images of past twoweeks fishing
  VMSgif(fisheryList,yrs=2004:2016,tail=14,pie.scale=7000,wd=800,ht=600,xlim=c(-60,-57.2),ylim=c(44,45.1),isobath=seq(50,500,50),bathy.source='bathy',poly.lst=list(VMSden.poly,data.frame(PID=1,col=rgb(0,0,0,0.2))))

  # version 2: weeky images of past months fishing, no lats, lons or bathy
  VMSgif(fisheryList,yrs=2004:2016,interval=7,tail=28,pie.scale=7000,wd=800,ht=600,xlim=c(-60,-57.2),ylim=c(44,45.1),ptcex=0.2,axes=F,xlab='',ylab='',isobath=NULL,ptcol=rgb(1,0,0,0.5),poly.lst=list(VMSden.poly,data.frame(PID=1,col=rgb(0,0,0,0.2))))

  # plot of VMS data
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","VMSLocations.pdf"),11,8)
  ClamMap2(xlim=c(-60,-57.2),ylim=c(44.1,45),isobath=seq(50,500,50),bathy.source='bathy')
  with(fisheryList$vms.data,points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  dev.off()

###CPUE data 

  # explore distribution of catch and effort data in order to set appropriate bounds to censor the data
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","CatchEffortDist.pdf"),8,8)

  par(mfrow=c(2,1))#,mar=c(0.2,0.2,0.2,0.2))  
  with(subset(processed.log.data,round_catch>0&round_catch<40000),hist(round_catch,breaks=100,xlim=c(0,40000),xlab="Reported Catch by Watch (kg)",main=''))
  abline(v=c(1500,30000),col='red',lwd=2)

  with(subset(processed.log.data,area>0&area<400000),hist(area,breaks=100,xlim=c(0,400000),xlab="Reported Effort by Watch (m2)",main=''))
  abline(v=c(15000,200000),col='red',lwd=2)

  dev.off()
    
## Grid Plots

  # Banquereau
  p$yrs= list(2004:2016)
  Totalgrid.out = FisheryGridPlot(fisheryList,p,vms=T,fn='totalVMS',boundPoly=Banq100,isobath=NULL,axes=F,ht=8,wd=11,xlab='',ylab='',effort.units='km2')
  Totalgrid.out = FisheryGridPlot(fisheryList,p,vms=T,fn='totalVMSv2',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
  save(Totalgrid.out,file=file.path( project.datadirectory("bio.surfclam"), "data", "griddedFisheryDataTotal.Rdata" ))
  p$yrs= list(2004:2006,2005:2007,2006:2008,2007:2009,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2016)
  grid3yr.out = FisheryGridPlot(fisheryList,p,vms=T,fn='3yrVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)

  p$yrs= 2004:2016

  #grid.out = FisheryGridPlot(fisheryList,p,fn='annualLog',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
  AnnGrid.out = FisheryGridPlot(fisheryList,p,vms=T,fn='annualVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
  write.csv(data.frame(Year=p$yrs,summarize.gridout(AnnGrid.out)),file=file.path( project.datadirectory("bio.surfclam"), "R", "SpatialExploitationSummary.csv"),row.names=F )

  save(AnnGrid.out,file=file.path( project.datadirectory("bio.surfclam"), "data", "griddedFisheryDataAnnual.Rdata" ))
  load(file=file.path( project.datadirectory("bio.surfclam"), "data", "griddedFisheryDataAnnual.Rdata" ))

  GridMapPlot(AnnGrid.out,yrs=2005:2016,xl=c(-59.85,-57.45),yl=c(44.3,44.8),graphic='pdf',info='catch')
  GridMapPlot(AnnGrid.out,yrs=2005:2016,xl=c(-59.85,-57.45),yl=c(44.3,44.8),graphic='pdf',info='effort')

  ## summary table of catch and effort data
  Years=1986:2016

  Ban.E = with(subset(processed.log.data,bank==1),tapply(area,year,sum,na.rm=T))
  Ban.C = with(subset(processed.log.data,bank==1),tapply(round_catch,year,sum,na.rm=T))
  Ban = data.frame(Year=as.numeric(names(Ban.C)),Ban.Catch = Ban.C/10^3, Ban.Effort = Ban.E/10^6, Ban.CPUE = Ban.C/Ban.E*1000)

  Grand.E = with(subset(processed.log.data,bank==2),tapply(area,year,sum,na.rm=T))
  Grand.C = with(subset(processed.log.data,bank==2),tapply(round_catch,year,sum,na.rm=T))
  Grand = data.frame(Year=as.numeric(names(Grand.C)),Grand.Catch = Grand.C/10^3, Grand.Effort = Grand.E/10^6, Grand.CPUE = Grand.C/Grand.E*1000)

  Table1 = subset(merge(Ban,Grand,all=T),Year%in%Years)
  write.csv(Table1,file.path( project.datadirectory("bio.surfclam"), "R","CatchEffort.csv"),row.names=F)

  CatchEffortPlot(Table1,graphic="pdf",wd=10)

  ## exploration of seasonal fishing patterns
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","SeasonalFishingPattern.pdf"),8,11)

  p$yrs= 2007:2016
  par(mfrow=c(3,3),mar=c(0,0,0,0))
  for (i in 1:length(p$yrs)) {
      fishing.season(subset(fisheryList$log.data,year%in%p$yrs[[i]]&bank==1,c('record_date','area')),smooth=0.01,title="")
      mtext("Relative effort",3,-2,cex=1.2,outer=T) 
    }
    # Apparently they fish pretty much all year round except for the winter of 2015, when presumably Banquereau was under 15ft of snow like everywhere else
  dev.off()






########### Survey ############

  # survey data
  surveyList = ProcessSurveyData()

  # Length - Weight relationship
  
  # 2010
  LenWt.data = subset(surveyList$Morphs,survey=="T12010-01",c("towid","length","total.weight"))
  names(LenWt.data)[3] = "weight"
  LenWt2010.fit=LengthWeight.lme(LenWt.data,random.effect='towid',b.par='estimate')
  LengthWeight.plt(LenWt2010.fit,lw=3,ht=8,wd=8,cx=1.5)

  l = seq(2.5,200,5)
  wal = l ^ LenWt2010.fit$B * LenWt2010.fit$A 

  # LengthFrequencies
  FisheryDataList = c(fisheryList,list(lf.data=lf.data))
  LengthFrequencies(FisheryDataList, DS="Fishery", bins=seq(0,200,5), Yrs=2009:2016, wal = wal, fn='BanqCatch', rel=F, ymax=60000,ylab="Number of Clams") 



#### Clam Habitat using VMS data and Areas ####
  
  # distribute Catch and Effort data over VMS locations
    vmslogdata = assignLogData2VMS(fisheryList, p)
    vmslogdata = subset(vmslogdata,EID%in%findPolys(vmslogdata,Banq100, maxRows = 1e+06)$EID)

  # create a polygon from vms density as a proxy for clam habitat
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","VMSdensity.pdf"),12,6)
  png(filename=file.path( project.datadirectory("bio.surfclam"),"figures",'VMSdensity.png'), 12,6, units="in", res=300)

  VMSden.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=30)
  VMSden.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=10)
  dev.off()

      load(file.path( project.datadirectory("bio.surfclam"),'data','VMSdensity.rdata'))
      write.csv(VMSden.poly,file.path( project.datadirectory("bio.surfclam"),'data',"VMSpolygons.csv"))

   
    ClamMap2("Ban",isobath=seq(50,500,50),bathy.source='bathy')
    addPolys(CWzones)
    addPolys(VMSden.poly,col=rgb(0,0,0,0.2))
    addLabels(data.frame(PID=1:10,label=1:10),polys=CWzones,placement="CENTROID",cex=2,font=2)
  
    
    pdf(file.path( project.datadirectory("bio.surfclam"), "figures","newAreas.pdf"),11,8)
      ClamMap2("Ban",isobath=seq(50,500,50),bathy.source='bathy')
      addPolys(new.areas)
      #addPolys(VMSden.poly,col=rgb(0,0,0,0.2),border=NA)
      addPolys(VMSden.poly,border=rgb(0,0,0,0.5))
      addLabels(data.frame(PID=1:5,label=1:5),polys=new.areas,placement="CENTROID",cex=2,font=2)
    dev.off()
   

  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","NewAreas2.pdf"),11,8)
    ClamMap2("Ban",isobath=NULL,axes=F,xlab='',ylab='')
    addPolys(VMSden.poly,col=rgb(0,0,0,0.2))
    #addPolys(CWzones)
    #addLabels(data.frame(PID=1:10,label=1:10),polys=CWzones,placement="CENTROID",cex=2,font=2)
    addPolys(new.areas)
    addLabels(data.frame(PID=1:5,label=1:5),polys=new.areas,placement="CENTROID",cex=2,font=2, col='black')
  dev.off()


  # sensitivity
  lvls=c(10,15,20,25,30,35,40,45,50)
  fishedarea=c()
  for(i in 1:length(lvls)){
   #pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0("VMSdensity",lvls[i],".pdf")),12,6)
   VMStmp.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=lvls[i])
   attr(VMStmp.poly,'projection')="LL"
    fishedarea[i] = calcArea(VMStmp.poly,1)$area
  #dev.off()
  }


############## Prepare Data for Production model ################


  oldlogdata = na.omit(subset(fisheryList$log.data, year<2003&bank==1&area>p$effort.threshold[1]&area<p$effort.threshold[2]&round_catch>p$catch.threshold[1]&round_catch<p$catch.threshold[2],  c("logrecord_id","lon_dd","lat_dd","round_catch","area","year","record_date")))
  names(oldlogdata) = names(vmslogdata)
  oldlogdata = subset(oldlogdata,EID%in%findPolys(oldlogdata,Banq100, maxRows = 1e+06)$EID)
 
  #VMSden.poly = joinPolys(VMSden.poly,junk,operation="DIFF")
  combineddata = rbind(oldlogdata,subset(vmslogdata,year>2002))
  combineddata$EID = 1:nrow(combineddata) 

  yrs = 1988:2016
  combineddata = subset(combineddata,year%in%yrs)

  # plot of CPUE data
  CPUEdata=SeasonalCPUE(combineddata,yrs,new.areas,lab='',graphic='pdf',wd=10,ht=10,col=rgb(0,0,0,0.3),pch=16,cex=0.5)
  CPUEdata=SeasonalCPUE(combineddata,yrs,new.areas,lab='doc',graphic='pdf',wd=7,ht=10,col=rgb(0,0,0,0.3),pch=16,cex=0.5)
  CPUEdata=SeasonalCPUE(combineddata,yrs,new.areas,lab='NoDaily',graphic='png',wd=7,ht=10,col=rgb(0,0,0,0.3),pch=16,cex=0.5,type='n')

###### create area summary tables 

  tyrs = 2004:2016
  Ban.C = with(subset(processed.log.data,bank==1&year%in%tyrs),tapply(round_catch,year,sum,na.rm=T))
  SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,new.areas,yrs=tyrs,effort.min=100000,r=5,n.min=7)
  SPMdataList = SPMdata$SPMdataList
  bumpup = Ban.C/1000/rowSums(SPMdataList$C)

  areaCatchesBU = sweep(SPMdataList$C,1,FUN='*',bumpup)

  areaCatches = rbind(areaCatchesBU,colMeans(areaCatchesBU))
  areaCatches = cbind(areaCatches,rowSums(areaCatches))
  dimnames(areaCatches)=list(c(tyrs,"Mean"),c(paste("Area",1:5),"Total"))
  write.csv(areaCatches,file.path( project.datadirectory("bio.surfclam"), "R","areaCatches.csv"))

  areaBiomass = SPMdataList$O
  areaBiomass = rbind(areaBiomass,colMeans(areaBiomass))
  areaBiomass = cbind(areaBiomass,rowSums(areaBiomass))
  dimnames(areaBiomass)=list(c(tyrs,"Mean"),c(paste("Area",1:5),"Total"))
  write.csv(areaBiomass,file.path( project.datadirectory("bio.surfclam"), "R","areaBiomass.csv"))

  TotalAreaDensity = read.csv(file.path( project.datadirectory("bio.surfclam"), "R","TotalAreaDensity2010.csv"))
  FishedAreaDensity = subset(TotalAreaDensity,EID%in%findPolys(TotalAreaDensity,VMSden.poly)$EID)


  attr(new.areas,"projection")<-"LL"
  totalareas = calcArea(new.areas) 
  keyf = findPolys(FishedAreaDensity,new.areas)
  keyt = findPolys(TotalAreaDensity,new.areas)
  fishedarea = merge(FishedAreaDensity,keyf)
   with(fishedarea,tapply(Z,PID,sum))
  totalarea = merge(TotalAreaDensity,keyt) 

  areaSummary = data.frame(totalareas,fished.area = SPMdata$Habitat, avg.annual.catch = colMeans(areaCatchesBU), total.catch.since.2004 = colSums(areaCatchesBU), biomass.survey.2010.total.area =with(totalarea,tapply(Z,PID,sum)),biomass.survey.2010 =with(fishedarea,tapply(Z,PID,sum)), biomass.cpue.2010 =SPMdataList$O['2010',], biomass.cpue.2016 = SPMdataList$O['2016',])
  areaSummary = rbind(areaSummary,colSums(areaSummary))
  areaSummary$PID[6] = "Total"
  write.csv(areaSummary,file.path( project.datadirectory("bio.surfclam"), "R","areaSummary.csv"),row.names=F)


################ model run 1: 

  yrs = 1988:2016
  # Spatial Production Model Data
  SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,new.areas,yrs=yrs,effort.min=100000,r=5,n.min=7,cv=T,err='sd',cv.min=0.01)

  log(SPMdata$meanCV^2+1)*3

  #SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,CWzones,yrs=yrs,effort.min=100000,r=5,n.min=7)
  #SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,new.areas,yrs=yrs,effort.min=100000,r=5,n.min=7,cv=F)
  SPMdataList = SPMdata$SPMdataList
  SPMdataList$yrs = yrs  

  SPMdataList$H = SPMdata$Habitat/mean(SPMdata$Habitat)
  #SPMdataList$CVW = 1/(SPMdataList$CV/SPMdata$meanCV)
  #SPMdataList$CVW = 1/(SPMdataList$CV/mean(SPMdataList$CV))

  NJ = SPMdataList$NJ
  NY = SPMdataList$NY


    # SPMdataList$CV[is.na(SPMdataList$CV)|SPMdataList$CV<=cv.min] = 1
    #SPMdataList$mu = log(SPMdataList$CV^2+1)
    #SPMdataList$imu2 = SPMdataList$mu^-2
    #SPMdataList$CV[is.na(SPMdataList$CV)&SPMdataList$CV==0] = 1
    #mu=log(SPMdataList$CV^2+1)*0.5
    #shape=matrix(3,NY,NJ)
    #rate=mu*shape
    # dist.plt("gamma",shape,shape*.7,xl=c(0,1))
    
  #dist.plt("gamma",3,0.4,xl=c(0,10))
 
  logK.u=log(apply(SPMdataList$O,2,max,na.rm=T))
  logB0.u=log(colMeans(SPMdataList$O,na.rm=T))
  #dist.plt("norm",11,4,xl=c(0,20))

  dist.plt("lnorm",log(0.2),0.7,xl=c(0,20))

  alpha=6
  #deff=.73
  deff=.45
  beta=(1-deff)*alpha/deff
  #dist.plt("beta",alpha,beta,xl=c(0,1))
  var=alpha*beta/((alpha+beta)^2*(alpha+beta+1))

    SPMpriors=list(
      logK=        list(a=12,      b=2,         d="dnorm",    i1=9,   i2=12,  l=1   ),    # carrying capacity
      logB0=       list(a=rep(11,NJ), b=rep(1,NJ), d="dnorm",    i1=10,   i2=9,  l=NJ  ),    # initial biomass
      #r=           list(a=3,       b=0.5,       d="dgamma",    i1=0.2, i2=0.1, l=1   ),    # intrinsic rate of increase
      r.u=         list(a=0,       b=1,         d="dunif",    i1=0.2, i2=0.1, l=1   ),    # intrinsic rate of increase
      r.sd=        list(a=-0.35,   b=0.08,      d="dlnorm",   i1=0.7, i2=0.5, l=1   ),    # intrinsic rate of increase
      q=           list(a=alpha,   b=beta,      d="dbeta",    i1=0.5, i2=0.8, l=1   ),    # clam dredge efficiency
      #tau=         list(a=0,       b=1,         d="dunif",   i1=2,  i2=3,  l=1   ),   # observation error (precision)
      itau2=       list(a=3,       b=0.4,   d="dgamma",   i1=15,  i2=30,  l=1   ),   # observation error (precision)
      #itau2=       list(a=shape,    b=rate,   d="dgamma",   i1=15,  i2=30,  l=NY*NJ   ),   # observation error (precision)
      sigma=       list(a=0,       b=1,         d="dunif",    i1=2,   i2=3,   l=1   )    # process error (SD)
      #isigma2=       list(a=3,       b=0.5,         d="dgamma",    i1=2,   i2=3,   l=1   )    # process error (precision)
    ) 

    SPmodel2.out=runBUGS("SPhyper1", SPMdataList, SPMpriors, SPMdataList$yrs, n = 600000, burn = 200000, thin = 10,debug=F,parameters=c(names(SPMpriors),'K','P','r','B0'),sw='jags',inits=F)
    save(SPmodel1.out,file=file.path( project.datadirectory("bio.surfclam"), "data", "SPM1output.Rdata" ))
    #load(file=file.path( project.datadirectory("bio.surfclam"), "data", "SPM1output.Rdata" ))
    SPmodel1.out$median
    Biomass=data.frame(Year=yrs,round(sweep(SPmodel1.out$median$P,2,SPmodel1.out$median$K,'*')))
    write.csv(Biomass,file.path( project.datadirectory("bio.surfclam"), "R","modelBiomass.csv"),row.names=F)

    # hyperprior
    SPMpriors$r = list(a=log(SPmodel1.out$median$r.u), b=SPmodel1.out$median$r.sd,   d="dlnorm",    i1=0.2, i2=0.1, l=1   )

  ## Plotting model results

    # plot fits to abundance indices 
    SPMfit.plt(SPmodel1.out, yrs=yrs, CI=T,CV=F,graphic='pdf',H = SPMdata$Habitat, ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=420)

    # plot the posterior distributions of the estimated parameters
    SPMpost.plt(SPmodel1.out,SPMpriors, graphic='pdf',nr=2,nc=3,wd=15,name='SPM1')
    #post.plt(SPmodel.out,SPmodelpriors,years=yrs, graphic='pdf',nr=2,nc=3,wd=15,multi=T)


    # plot biomass 
    SPMbiomass.plt(SPmodel1.out, yrs=yrs, CI=T,graphic='pdf',ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=320)

    # exploitation
    SPMexploitation.plt(SPmodel1.out, yrs=yrs, CI=T,graphic='pdf',ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=0.32)


    # plot stocastic reference points
    refs=SPMRefpts(SPmodel1.out,col='grey', graphic='pdf')

    frefs = list(c(0.026,0.045)/0.09,c(0.026,0.045)/0.09,c(0.026,0.045)/0.09,c(0.026,0.045)/0.09,c(0.026,0.045)/0.09)
    cpueRef=70*SPMdata$Habitat/refs$BMSY/SPmodel1.out$median$q
    brefs = list(c(0.8,0.4,cpueRef[1]),c(0.8,0.4,cpueRef[2]),c(0.8,0.4,cpueRef[3]),c(0.8,0.4,cpueRef[4]),c(0.8,0.4,cpueRef[5]))

    # phase plots

    SPMPhaseplts(SPmodel1.out,ymax=2.2, graphic='png',vline=brefs,hline=frefs,vcol=c('gold','red','green'))

    Brefs = data.frame(cbind(do.call("rbind",lapply(as.list(refs$BMSY),'*',c(0.4,0.8))),70*SPMdata$Habitat/SPmodel1.out$median$q))
    names(Brefs) = c("LRP", "USR", "cpue70")
    Brefs = rbind(Brefs,colSums(Brefs))


   # plot biomass 
    SPMbiomass.plt(SPmodel1.out, yrs=yrs, CI=T,graphic='png',ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=320,refs=Brefs/1000,refcol=c('red','gold','green'),total=T)

    Blist=list()
    for(j in 1:5){
        Bposts = sweep(SPmodel1.out$sims.list$P[,,j],1,FUN='*',SPmodel1.out$sims.list$K[,j]/1000)

      Blist[[j]] =  quantile(Bposts[,29],  c(0.025,0.5,0.975))
    }
    Biomass2016 = data.frame(do.call("rbind",Blist))
    Biomass2016 = rbind(Biomass2016,colSums(Biomass2016))

    Emed = list()
    for(j in 1:5){

    C=model.out$data$C[,j]
    Emed[[j]] = C/(model.out$median$P[,j]*model.out$median$K[j])
  }
    Emed = data.frame(do.call("rbind",Emed))

  png(filename=file.path( project.datadirectory("bio.surfclam"), "figures","TACs.png"), width=7, height=6, units="in", res=300)
  
  tBiomass=rowSums(Biomass[,-1])
  plot(yrs,tBiomass*(1-exp(-0.045)),type='l',ylim=c(0,65000),col='gold',lwd=2,xlim=c(1999,2016),ylab="TAC (t)",xlab='')
  lines(yrs,tBiomass*(1-exp(-0.026)),col='green',lwd=2)
  lines(yrs,tBiomass*(1-exp(-0.09)),col='red',lwd=2)
  abline(h=24000)
  abline(h=50000,lty=3)
  legend('topleft', c("High (Fmsy)", "Medium (0.5Fmsy)", "Low (0.33M)"),title="Risk",lty=1,col=c('red','gold','green'),bg='white')
  dev.off()


  Fref = c(0.045,0.026)
  tacs=rbind(Biomass[29,-1]*(1-exp(-Fref[1])),  Biomass[29,-1]*(1-exp(-Fref[2])))
  data.frame(Fref,tacs,total=rowSums(tacs))


Biomass2016

################################ GRand Bank ################################


  # GrandBank
  p$yrs= list(2004:2016)
  p$bank= "Grand"
  p$Min_lon = -51.5
  p$Max_lon = -48.5
  p$Min_lat = 43.0
  p$Max_lat = 46.5
  #GrandTotalgrid.out = FisheryGridPlot(fisheryList,p,vms=T,fn='GrandTotalVMS',isobath=seq(50,500,50),nafo='all',lg.place="topleft",ht=8,wd=6,outsideBorder=T)#,aspr=1)

  GrandTotalgrid.out = FisheryGridPlot(fisheryList,p,vms=T,fn='GrandTotalVMS',isobath=110,lg.place="topleft",ht=8,wd=6,outsideBorder=T,axes=F,xlab='',ylab='')#,aspr=1)


  p$yrs= 2004:2016
  GrandAnnGrid.out = FisheryGridPlot(fisheryList,p,vms=T,fn='annualVMS',isobath=110,lg.place="topleft",ht=8,wd=6,outsideBorder=T,axes=F,xlab='',ylab='')#,aspr=1)
  write.csv(data.frame(Year=p$yrs,summarize.gridout(GrandAnnGrid.out)),file=file.path( project.datadirectory("bio.surfclam"), "R", "GrandSpatialExploitationSummary.csv"),row.names=F )

  save(GrandAnnGrid.out,file=file.path( project.datadirectory("bio.surfclam"), "data", "GrandgriddedFisheryDataAnnual.Rdata" ))
  load(file=file.path( project.datadirectory("bio.surfclam"), "data", "GrandgriddedFisheryDataAnnual.Rdata" ))

  grandvmslogdata = assignLogData2VMS(fisheryList, p)
  grandvmslogdata = subset(grandvmslogdata,X>p$Min_lon&X<p$Max_lon&Y>p$Min_lat&Y<p$Max_lat)


  ClamMap2('Grand',isobath=seq(50,500,50))

  rect(Min_long,Min_lat,Max_long,Max_lat)
  with(subset(processed.log.data,year==2013&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(0,0,1,0.2)))
  with(surveyList$surveyData,points(slon,slat,pch=16,cex=0.2,col=rgb(0,1,0,0.2)))




  # plot of VMS data
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","VMSLocationsGrand.pdf"),8,11)
  ClamMap2('Grand',isobath=seq(50,500,50),bathy.source='bathy')
  with(fisheryList$vms.data,points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  dev.off()
 
 # create a polygon from vms density as a proxy for clam habitat
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","VMSdensityGrand.pdf"),6,12)
  GrandVMSden.poly = vmsDensity(grandvmslogdata,sig=0.2,res=0.1,lvl=20,lab="Grand",zone=22)
  dev.off()

 # plot of VMS data
  pdf(file.path( project.datadirectory("bio.surfclam"), "figures","VMSAreasGrand.pdf"),8,11)
  ClamMap2('Grand',isobath=seq(50,500,50))
  addPolys(GrandVMSden.poly,col=rgb(0,0,0,0.2))
  dev.off()


