###############################################################################
##
##
##  Artic Surf Clam Framework Script
##
##  June 2016
##
##  Brad Hubley
##  Susan Heaslip
##
##
##
###############################################################################
# To run in ecomod, run the following commands

loadfunctions(c("offshoreclams","lobster","utility","spacetime","model.fishery.general"))

RLibrary( "PBSmapping", "lubridate", "trip" ,"spatstat","TeachingDemos") # Load required packages


## Load Data

update.data=FALSE # TRUE accesses data from database if on a DFO windows machine


  # log data
  #log.data <- GetLogData(update=update.data)
  processed.log.data <- ProcessLogData(GetLogData(update=update.data))
  # save(processed.log.data,file=file.path( project.datadirectory("offshoreclams"), "data", "processedLogdata.Rdata" ))

  #VMS data
  #vms.data <- GetVMSData(update=update.data)
  fisheryList <- ProcessVMSData(GetVMSData(update=update.data),processed.log.data)
  #processed.log.data = fisheryList$log.data
  processed.vms.data = fisheryList$vms.data
  load(file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryDataTotal.Rdata" ))

  # length frequency data
  lf.data <- GetLFData(update=update.data)
  

  # survey data
  surveyList <- ProcessSurveyData()

  # bounding polygon at 100m isobath for Banquereau
  c100 <- read.table(file.path( project.datadirectory("polygons"), "data","Basemaps","Marine","Bathymetry","CHS100.ll"),header=T)
  Banq100 <- na.omit(subset(c100,SID==2392)) # 100m isobath for Banqureau

  # Clearwater Zones
  #CWzones <- read.table(file.path( project.datadirectory("offshoreclams"), "data","maps","polyBBrot.txt"),header=F)
  #names(CWzones) = c("PID","X","Y")
  #CWzones$POS = 1:nrow(CWzones)
  #write.csv(CWzones[,c("PID","POS","X","Y")],file.path( project.datadirectory("offshoreclams"), "data","maps","CWzones.csv"),row.names=F)
  CWzones <- read.csv(file.path( project.datadirectory("offshoreclams"), "data","maps","CWzones.csv"))

## parameters

  p=list()
  p$bank= "Ban"
  p$yrs= list(2004:2015)
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
  with(subset(processed.log.data,year==2015&area>0),points(lon_dd,lat_dd,pch=16,cex=0.2,col=rgb(1,0,0,0.2)))
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


  # VMS Data
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","FishingLocations.pdf"),11,8)
  for (i in 2002:2015) {
  ClamMap2('Ban',isobath=seq(50,500,50),title=i,bathy.source='bathy',nafo='all')
  with(subset(processed.log.data,year==i&area>0),points(lon_dd,lat_dd,pch=16,cex=0.5,col=rgb(1,0,0,0.2)))
  with(subset(processed.vms.data,year==i),points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  legend('bottomright',c("logs","vms"),pch=16,col=c(rgb(1,0,0,0.2),rgb(0,0,0,.1)),cex=c(0.5,0.2))
  }
  dev.off()

  # VMS GIF!!!
  # version 1: daily images of past twoweeks fishing
  VMSgif(fisheryList,yrs=2004:2015,tail=14,pie.scale=7000,wd=800,ht=600,xlim=c(-60,-57.2),ylim=c(44,45.1),isobath=seq(50,500,50),bathy.source='bathy',poly.lst=list(VMSden.poly,data.frame(PID=1,col=rgb(0,0,0,0.2))))

  # version 2: weeky images of past months fishing, no lats, lons or bathy
  VMSgif(fisheryList,yrs=2004:2015,interval=7,tail=28,pie.scale=7000,wd=800,ht=600,xlim=c(-60,-57.2),ylim=c(44,45.1),ptcex=0.2,axes=F,xlab='',ylab='',isobath=NULL,ptcol=rgb(1,0,0,0.5),poly.lst=list(VMSden.poly,data.frame(PID=1,col=rgb(0,0,0,0.2))))

  # plot of VMS data
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","VMSLocations.pdf"),11,8)
  ClamMap2(xlim=c(-60,-57.2),ylim=c(44.1,45),isobath=seq(50,500,50),bathy.source='bathy')
  with(fisheryList$vms.data,points(lon,lat,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  dev.off()

###CPUE data

  # explore distribution of catch and effort data in order to set appropriate bounds to censor the data
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","CatchEffortDist.pdf"),8,8)

  par(mfrow=c(2,1))#,mar=c(0.2,0.2,0.2,0.2))  
  with(subset(processed.log.data,round_catch>0&round_catch<40000),hist(round_catch,breaks=100,xlim=c(0,40000),xlab="Reported Catch by Watch (kg)",main=''))
  abline(v=c(1500,30000),col='red',lwd=2)

  with(subset(processed.log.data,area>0&area<400000),hist(area,breaks=100,xlim=c(0,400000),xlab="Reported Effort by Watch (m2)",main=''))
  abline(v=c(15000,200000),col='red',lwd=2)

  dev.off()
    
## Grid Plots

  # Banquereau
  p$yrs= list(2004:2015)
  Totalgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='totalVMS',boundPoly=Banq100,isobath=NULL,axes=F,ht=8,wd=11,xlab='',ylab='',effort.units='km2')
  Totalgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='totalVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
  save(Totalgrid.out,file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryDataTotal.Rdata" ))
  p$yrs= list(2004:2006,2005:2007,2006:2008,2007:2009,2008:2010,2009:2011,2010:2012,2011:2013,2012:2014,2013:2015)
  3yrgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='3yrVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)

  p$yrs= 2004:2015

  #grid.out <- FisheryGridPlot(fisheryList,p,fn='annualLog',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
  AnnGrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='annualVMS',boundPoly=Banq100,isobath=seq(50,500,50),bathy.source='bathy',nafo='all')#,aspr=1)
  write.csv(data.frame(Year=p$yrs,summarize.gridout(AnnGrid.out)),file=file.path( project.datadirectory("offshoreclams"), "R", "SpatialExploitationSummary.csv"),row.names=F )

  save(AnnGrid.out,file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryDataAnnual.Rdata" ))
  load(file=file.path( project.datadirectory("offshoreclams"), "data", "griddedFisheryDataAnnual.Rdata" ))

  GridMapPlot(AnnGrid.out,yrs=2004:2015,xl=c(-59.85,-57.45),yl=c(44.3,44.8),graphic='R',info='catch')
  GridMapPlot(AnnGrid.out,yrs=2004:2015,xl=c(-59.85,-57.45),yl=c(44.3,44.8),graphic='pdf',info='effort')


  # GrandBank
  p$yrs= list(2004:2015)
  p$bank= "Grand"
  p$Min_lon = -51.5
  p$Max_lon = -48.5
  p$Min_lat = 43.0
  p$Max_lat = 46.5
  #GrandTotalgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='GrandTotalVMS',isobath=seq(50,500,50),nafo='all',lg.place="topleft",ht=8,wd=6,outsideBorder=T)#,aspr=1)

  GrandTotalgrid.out <- FisheryGridPlot(fisheryList,p,vms=T,fn='GrandTotalVMS',isobath=110,lg.place="topleft",ht=8,wd=6,outsideBorder=T,axes=F,xlab='',ylab='')#,aspr=1)


  ## summary table of catch and effort data
  Years=1986:2015

  Ban.E = with(subset(processed.log.data,bank==1),tapply(area,year,sum,na.rm=T))
  Ban.C = with(subset(processed.log.data,bank==1),tapply(round_catch,year,sum,na.rm=T))
  Ban = data.frame(Year=as.numeric(names(Ban.C)),Ban.Catch = Ban.C/10^3, Ban.Effort = Ban.E/10^6, Ban.CPUE = Ban.C/Ban.E*1000)

  Grand.E = with(subset(processed.log.data,bank==2),tapply(area,year,sum,na.rm=T))
  Grand.C = with(subset(processed.log.data,bank==2),tapply(round_catch,year,sum,na.rm=T))
  Grand = data.frame(Year=as.numeric(names(Grand.C)),Grand.Catch = Grand.C/10^3, Grand.Effort = Grand.E/10^6, Grand.CPUE = Grand.C/Grand.E*1000)

  Table1 = merge(Ban,Grand,all=T)
  write.csv(Table1,file.path( project.datadirectory("offshoreclams"), "R","CatchEffort.csv"),row.names=F)

  CatchEffortPlot(Table1,graphic="R")

  ## exploration of seasonal fishing patterns
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SeasonalFishingPattern.pdf"),8,11)

  p$yrs= 2007:2015
  par(mfrow=c(3,3),mar=c(0,0,0,0))
  for (i in 1:length(p$yrs)) {
      fishing.season(subset(fisheryList$log.data,year%in%p$yrs[[i]]&bank==1,c('record_date','area')),smooth=0.01,title="")
      mtext("Relative effort",3,-2,cex=1.2,outer=T) 
    }
    # Apparently they fish pretty much all year round except for the winter of 2015, when presumably Banquereau was under 15ft of snow like everywhere else
  dev.off()


  # distribution of surf clams catch from log data
  p$yrs=list(2004:2010,2011:2015)
  b=1
    
    # or use this
   # distribute Catch and Effort data over VMS locations
    vmslogdata = assignLogData2VMS(fisheryList, p)
    vmslogdata = subset(vmslogdata,EID%in%findPolys(vmslogdata,Banq100, maxRows = 1e+06)$EID)

  pdf(file.path( project.datadirectory("offshoreclams"), "figures","TotalRemovals.pdf"),8,11)

  for(i in 1:length(p$yrs)){
    
    # interpolate abundance
    interp.data <- na.omit(subset(processed.log.data,year%in%p$yrs[[i]]&bank==b&lat_dd>Min_lat[b]&lat_dd<Max_lat[b]&lon_dd>Min_long[b]&lon_dd<Max_long[b],c('logrecord_id','lon_dd','lat_dd','round_catch','area')))
    interp.data <-subset(vmslogdata,year%in%p$yrs[[i]],c('EID','X','Y','C','A')))
    catch.contours <- interpolation(interp.data[,-5],ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=F,res=1/111.12,sres=1/111.12,smooth=T,smooth.fun=sum)
    effort.contours <- interpolation(interp.data[,-4],ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=F,res=1/111.12,sres=1/111.12,smooth=T,smooth.fun=sum)

    # define contour lines
    print(clam.contours$str.def)
    # 0.000    9998.709   28722.120   82390.560  202708.380 2950358.365
    lvls=c(5000, 10000, 50000, 100000, 200000, 500000, 1000000)

    # generate contour lines
    cont.lst<-contour.gen(clam.contours$image.dat,lvls,Banq100,col="YlGn",colorAdj=1)

    # plot Map
    ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all',contours=cont.lst,title=paste("Banqureau Surf Clam Removals",min(p$yrs[[i]]),'-',max(p$yrs[[i]])))
    #points(lat_dd~lon_dd,interp.data,pch=16,cex=0.1,col=rgb(0,0,0,0.1))
    ContLegend("bottomright",lvls=lvls/1000,Cont.data=cont.lst$Cont.data,title=expression(t/NM^2),inset=0.02,cex=0.8,bty='n')
  }
  dev.off()






########### Survey ############

  ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
  with(subset(surveyList$surveyData,year==2010),segments(slon, slat, elon, elat,col='red'))
  with(subset(surveyList$surveyData,year==2010),points(slon, slat,pch=16,cex=0.3,col='red'))
  with(subset(surveyList$surveyData,year==2004),segments(slon, slat, elon, elat,col='green'))
  with(subset(surveyList$surveyData,year==2004),points(slon, slat,pch=16,cex=0.3,col='green'))

  # comparing recorded tow distance with the distance between start and end points
  plot(length~dist_m,surveyList$surveyData)
  abline(0,1)


  # Length - Weight relationship
  # 2004
  LenWt.data = subset(surveyList$Morphs,survey=="CK2004-01",c("towid","length","total.weight"))
  names(LenWt.data)[3] <- "weight"
  LenWt2004.fit<-LengthWeight.lme(LenWt.data,random.effect='towid',b.par='estimate')
  LengthWeight.plt(LenWt2004.fit,lw=3,ht=8,wd=8,cx=1.5)

  # 2010
  LenWt.data = subset(surveyList$Morphs,survey=="T12010-01",c("towid","length","total.weight"))
  names(LenWt.data)[3] <- "weight"
  LenWt2010.fit<-LengthWeight.lme(LenWt.data,random.effect='towid',b.par='estimate')
  LengthWeight.plt(LenWt2010.fit,lw=3,ht=8,wd=8,cx=1.5)

  l = seq(2.5,200,5)
  wal = l^LenWt2010.fit$B * LenWt2010.fit$A 

  # LengthFrequencies
  FisheryDataList = c(fisheryList,list(lf.data=lf.data))
  LengthFrequencies(FisheryDataList, DS="Fishery", bins=seq(0,200,5), Yrs=2009:2013, wal = wal, fn='BanqCatch', rel=F, ymax=40000,ylab="Number of Clams") 




 # create new area polygons
    vmslogdata = assignLogData2VMS(fisheryList, p)
    vmslogdata = subset(vmslogdata,EID%in%findPolys(vmslogdata,Banq100, maxRows = 1e+06)$EID)
    VMSden.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=30)

  # distribution of surf clams from survey
  TotalAreaDensity = list()
  FishedAreaDensity = list()

  TotalAreaBiomass = list()
  FishedAreaBiomass = list()

  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SurveyDensity.pdf"),11,8)


  for(i in c(2004,2010)){
    #i=2010
    # interpolate abundance
    interp.data <- na.omit(subset(surveyList$surveyData,year==i&towtype%in%c(1)&towquality%in%c(1,2),c('EID','X','Y','stdcatch')))
    clam.contours<-interpolation(interp.data,ticks='define',place=3,nstrata=5,str.min=0,interp.method='gstat',blank=T,res=1/111.12,smooth=F,idp=5,blank.dist=0.1)


    # define contour lines
    print(clam.contours$str.def)
    lvls=c(1, 15, 35, 75, 150, 300)

      Y<-sort(rep(clam.contours$image.dat$y,length(clam.contours$image.dat$x)))
      X<-rep(clam.contours$image.dat$x,length(clam.contours$image.dat$y))
      TotalAreaDensity[[i]] = data.frame(EID=1:length(X),X=X,Y=Y,Z=as.vector(clam.contours$image.dat$z))
      FishedAreaDensity[[i]] = subset(TotalAreaDensity[[i]],EID%in%findPolys(TotalAreaDensity[[i]],VMSden.poly)$EID)


    TotalAreaBiomass[[i]] = sum(clam.contours$image.dat$z,na.rm=T)
    FishedAreaBiomass[[i]] = sum( FishedAreaDensity[[i]]$Z,na.rm=T)

    # generate contour lines
    cont.lst<-contour.gen(clam.contours$image.dat,lvls,Banq100,col="YlGn",colorAdj=1)

    # plot Map
    ClamMap2('Ban',isobath=seq(50,500,50),bathy.source='bathy',nafo='all',contours=cont.lst,title=paste("Banqureau Surf Clam Survey Density",i))
    points(Y~X,interp.data,pch=16,cex=0.5)
    #addPolys(VMSden.poly,border=rgb(1,0,0,0.5))
    ContLegend("bottomright",lvls=lvls,Cont.data=cont.lst$Cont.data,title=expression(t/km^2),inset=0.02,cex=0.8,bty='n')
  }
  dev.off()


  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SurveyCPUEcompare.pdf"),11,8)

  for(i in c(2004,2010)){

    # compare survey to fishery catch rates in 2010

    grid.data <- na.omit(subset(surveyList$surveyData,year==i&towtype%in%c(1,4)&towquality%in%c(1,2),c('EID','X','Y','stdcatch')))
    surveygrids<-gridData(grid.data,lvls=c(1, 30, 75, 150, 300, 600),bcol="YlGn",FUN=mean,border=NA,grid.size=p$grid.size,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)

    fisherycpue = AnnGrid.out$grid.polyData$cpue[[as.character(i)]]

    fisherycpue$FisheryClamDensity = fisherycpue$Z*1000
    surveycpue = surveygrids$polyData
    surveycpue$SurveyClamDensity = surveycpue$Z
    comparison.data = merge(fisherycpue[,c("PID","SID","FisheryClamDensity")],surveycpue[,c("PID","SID","SurveyClamDensity")])
    Compare.points = subset(grid.data,EID%in%subset(findPolys(grid.data,surveygrids$polys),paste(PID,SID)%in%with(comparison.data,paste(PID,SID)))$EID)

    plot.comparison = function(comparison.data){
      plot(SurveyClamDensity~FisheryClamDensity,comparison.data,xlim=c(0,515),ylim=c(0,515))
      #abline(lm(SurveyClamDensity~FisheryClamDensity-1,comparison.data))
      abline(a=0,b=1)
    }

    #ClamMap2(ylim=c(43.6,45.1), xlim=c(-60.2,-56.8),poly.lst=list(AnnGrid.out$grid,fisherycpue),title=paste("Surf Clam CPUE Comparison",i),isobath=seq(50,500,50),bathy.source='bathy',nafo='all')
    ClamMap2(ylim=c(43.6,45.1), xlim=c(-60.2,-56.8),poly.lst=list(AnnGrid.out$grid,fisherycpue),title=paste("Surf Clam CPUE Comparison",i),isobath=NULL,axes=F,xlab='',ylab='')
    #ContLegend("topleft",lvls=p$cpue.levels*1000,Cont.data=list(AnnGrid.out$grid,fisherycpue2010),title=expression(CPUE (t/km^2)),inset=0.02,cex=0.8,bg='white')
    points(Y~X,Compare.points,pch=21,bg='red')
    points(Y~X,grid.data)
    subplot(plot.comparison(comparison.data),x=-57.3,y=44.08,size=c(2,1.8))
  }
  dev.off()

#### new areas ####
  
  # create new area polygons
    vmslogdata = assignLogData2VMS(fisheryList, p)
    vmslogdata = subset(vmslogdata,EID%in%findPolys(vmslogdata,Banq100, maxRows = 1e+06)$EID)
    VMSden.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=30)
   
    ClamMap2("Ban",isobath=seq(50,500,50),bathy.source='bathy')
    addPolys(CWzones)
    addPolys(VMSden.poly,col=rgb(0,0,0,0.2))
    addLabels(data.frame(PID=1:10,label=1:10),polys=CWzones,placement="CENTROID",cex=2,font=2)

    outline = joinPolys(CWzones,operation="UNION")
    #addPolys(outline,border='red')

    with(CWzones,points(X,Y))

    area1 = joinPolys(subset(CWzones,PID%in%c(1,2,3)),operation="UNION")
    area1 = rbind(area1[1:17,],data.frame(PID=1,POS=18,X=-58.74802,Y=44.33333),area1[20:24,])
    area1$POS=1:nrow(area1)
    area1$PID=1
    addPolys(area1,col=rgb(0,1,0,0.2))

    area2 = joinPolys(subset(CWzones,PID%in%c(3,4,5,8)),operation="UNION")
    
    area3 = joinPolys(subset(CWzones,PID%in%c(5,6,7)),operation="UNION")
    area3 = rbind(area3[1,],area3[1:3,],
      data.frame(PID=1,POS=5,X=-58.5,Y=44.33333),
      area3[c(6:20),],
      CWzones[123,],
      area3[22,],
      data.frame(PID=1,POS=99,X=-57.95,Y=44.53))
    area3$POS=1:nrow(area3)
    area3$PID=3
    area3[1,]$X=-58.11
    addPolys(area3,col=rgb(1,1,0,0.2))


    area4 = joinPolys(subset(CWzones,PID%in%c(6,8)),operation="UNION")
    area4 = rbind(area4[c(7,11:16,21),],data.frame(PID=1,POS=22,X=-57.95,Y=44.53))
    area4$POS=1:nrow(area4)
    area4$PID=4
    area4[7:8,]$X=-58.11
    addPolys(area4,col=rgb(0,1,1,0.2))
   
    area5 = joinPolys(subset(CWzones,PID%in%c(8,9,10)),operation="UNION")
    area5 = area5[c(1,2,13,15:21),]
    area5$POS=1:nrow(area5)
    area5$PID=5
    addPolys(area5,col=rgb(1,0,0,0.2))
        
    area2 = joinPolys(outline,joinPolys(rbind(area1,area3,area4,area5),operation="UNION"),operation="DIFF")
    area2$PID=2
    area2$POS=1:nrow(area2)

     new.areas = rbind(area1,area2,area3,area4,area5)
     write.csv(new.areas,file.path( project.datadirectory("offshoreclams"), "R","newareas.csv"),row.names=F)
  
    pdf(file.path( project.datadirectory("offshoreclams"), "figures","newAreas.pdf"),11,8)
      ClamMap2("Ban",isobath=seq(50,500,50),bathy.source='bathy')
      addPolys(area1,col=rgb(0,1,0,0.2))
      addPolys(area2,col=rgb(1,0,1,0.2))
      addPolys(area3,col=rgb(1,1,0,0.2))
      addPolys(area4,col=rgb(0,1,1,0.2))
      addPolys(area5,col=rgb(1,0,0,0.2))
      #addPolys(VMSden.poly,col=rgb(0,0,0,0.2),border=NA)
      addPolys(VMSden.poly,border=rgb(0,0,0,0.5))
      addLabels(data.frame(PID=1:5,label=1:5),polys=new.areas,placement="CENTROID",cex=2,font=2)
    dev.off()
   
  new.areas  = read.csv(file.path( project.datadirectory("offshoreclams"), "R","newareas.csv"))

  attr(new.areas,"projection")<-"LL"
  totalareas = calcArea(new.areas) 
  
  combineddata = rbind(oldlogdata,vmslogdata)
  combineddata$EID = 1:nrow(combineddata) 


###### create area summary tables 

  yrs = 2004:2015
  Ban.C = with(subset(processed.log.data,bank==1&year%in%yrs),tapply(round_catch,year,sum,na.rm=T))
  SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,new.areas,yrs=yrs,effort.min=100000,r=5,n.min=7)
  SPMdataList = SPMdata$SPMdataList
  bumpup = Ban.C/1000/rowSums(SPMdataList$C)

  areaCatchesBU = sweep(SPMdataList$C,1,FUN='*',bumpup)

  areaCatches = rbind(areaCatchesBU,colMeans(areaCatchesBU))
  areaCatches = cbind(areaCatches,rowSums(areaCatches))
  dimnames(areaCatches)<-list(c(yrs,"Mean"),c(paste("Area",1:5),"Total"))
  write.csv(areaCatches,file.path( project.datadirectory("offshoreclams"), "R","areaCatches.csv"))

  areaBiomass = SPMdataList$O
  areaBiomass = rbind(areaBiomass,colMeans(areaBiomass))
  areaBiomass = cbind(areaBiomass,rowSums(areaBiomass))
  dimnames(areaBiomass)<-list(c(yrs,"Mean"),c(paste("Area",1:5),"Total"))
  write.csv(areaBiomass,file.path( project.datadirectory("offshoreclams"), "R","areaBiomass.csv"))

  keyf = findPolys(fishedarea,new.areas)
  keyt = findPolys(totalarea,new.areas)
  fishedarea = merge(fishedarea,keyf)
   with(fishedarea,tapply(Z,PID,sum))
  totalarea = merge(totalarea,keyt) 

  areaSummary = data.frame(totalareas,fished.area = SPMdata$Habitat, avg.annual.catch = colMeans(areaCatchesBU), total.catch.since.2004 = colSums(areaCatchesBU), biomass.survey.2010.total.area =with(totalarea,tapply(Z,PID,sum)),biomass.survey.2010 =with(fishedarea,tapply(Z,PID,sum)), biomass.cpue.2010 =SPMdataList$O['2010',], biomass.cpue.2015 = SPMdataList$O['2015',])
  areaSummary = rbind(areaSummary,colSums(areaSummary))
  areaSummary$PID[6] = "Total"
  write.csv(areaSummary,file.path( project.datadirectory("offshoreclams"), "R","areaSummary.csv"),row.names=F)





############## Production model ################

  loadfunctions(c("offshoreclams","lobster","utility","spacetime","model.fishery.general"))
  

  #interp.data <- na.omit(subset(surveyList$surveyData,year==i&towtype%in%c(1,4)&towquality==1,c('EID','X','Y','stdcatch')))

  #res = with(interp.data,spacetime.variogram(data.frame(X,Y),stdcatch,methods="gstat")) 
   
  # distribute Catch and Effort data over VMS locations
  vmslogdata = assignLogData2VMS(fisheryList, p)
  vmslogdata = subset(vmslogdata,EID%in%findPolys(vmslogdata,Banq100, maxRows = 1e+06)$EID)

  oldlogdata = na.omit(subset(fisheryList$log.data, year<2003&bank==1&area>p$effort.threshold[1]&area<p$effort.threshold[2]&round_catch>p$catch.threshold[1]&round_catch<p$catch.threshold[2],  c("logrecord_id","lon_dd","lat_dd","round_catch","area","year","record_date")))
  names(oldlogdata) = names(vmslogdata)
  oldlogdata = subset(oldlogdata,EID%in%findPolys(oldlogdata,Banq100, maxRows = 1e+06)$EID)
 
  # create a polygon from vms density as a proxy for clam habitat
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","VMSdensity.pdf"),12,6)
  VMSden.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=20)
  dev.off()
  new.areas  = read.csv(file.path( project.datadirectory("offshoreclams"), "R","newareas.csv"))

  # sensitivity
  lvls=c(10,15,20,25,30,35,40,45,50)
  fishedarea=c()
  for(i in 1:length(lvls)){
   pdf(file.path( project.datadirectory("offshoreclams"), "figures",paste0("VMSdensity",lvls[i],".pdf")),12,6)
   VMSden.poly = vmsDensity(vmslogdata,sig=0.2,res=0.1,lvl=lvls[i])
    fishedarea[i] = calcArea(VMSden.poly,1)$area
  dev.off()
  }

  #VMSden.poly = joinPolys(VMSden.poly,junk,operation="DIFF")

  pdf(file.path( project.datadirectory("offshoreclams"), "figures","NewAreas2.pdf"),11,8)
    ClamMap2("Ban",isobath=NULL,axes=F,xlab='',ylab='')
    addPolys(VMSden.poly,col=rgb(0,0,0,0.2))
    #addPolys(CWzones)
    #addLabels(data.frame(PID=1:10,label=1:10),polys=CWzones,placement="CENTROID",cex=2,font=2)
    addPolys(new.areas)
    addLabels(data.frame(PID=1:5,label=1:5),polys=new.areas,placement="CENTROID",cex=2,font=2, col='black')
  dev.off()

  combineddata = rbind(oldlogdata,subset(vmslogdata,year>2002))
  combineddata$EID = 1:nrow(combineddata) 

  yrs = 1988:2015

  # plot of CPUE data
  CPUEdata=SeasonalCPUE(combineddata,yrs,new.areas,lab='',graphic='R',wd=10,ht=10,col=rgb(0,0,0,0.3),pch=16,cex=0.5)

################ model run 1: 

  SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,new.areas,yrs=yrs,effort.min=100000,r=5,n.min=7,cv=T,err='sd',cv.min=0.01)
  log(SPMdata$meanCV^2+1)*3

  #SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,CWzones,yrs=yrs,effort.min=100000,r=5,n.min=7)
  #SPMdata = SPMsetup(combineddata,Totalgrid.out,VMSden.poly,new.areas,yrs=yrs,effort.min=100000,r=5,n.min=7,cv=F)
  SPMdataList = SPMdata$SPMdataList

  SPMdataList$H = SPMdata$Habitat/mean(SPMdata$Habitat)
  #SPMdataList$CVW = 1/(SPMdataList$CV/SPMdata$meanCV)
  #SPMdataList$CVW = 1/(SPMdataList$CV/mean(SPMdataList$CV))

  NJ = SPMdataList$NJ
  NY = SPMdataList$NY


    # SPMdataList$CV[is.na(SPMdataList$CV)|SPMdataList$CV<=cv.min] <- 1
    #SPMdataList$mu <- log(SPMdataList$CV^2+1)
    #SPMdataList$imu2 <- SPMdataList$mu^-2
    #SPMdataList$CV[is.na(SPMdataList$CV)&SPMdataList$CV==0] <- 1
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

    SPmodel1.out<-runBUGS("SPhyper1", SPMdataList, SPMpriors, SPMdataList$yrs, n = 600000, burn = 200000, thin = 10,debug=F,parameters=c(names(SPMpriors),'K','P','r','B0'),sw='jags',inits=F)
    save(SPmodel1.out,file=file.path( project.datadirectory("offshoreclams"), "data", "SPM1output.Rdata" ))
    #load(file=file.path( project.datadirectory("offshoreclams"), "data", "SPM1output.Rdata" ))
    SPmodel1.out$median
    Biomass=data.frame(Year=yrs,round(sweep(SPmodel1.out$median$P,2,SPmodel1.out$median$K,'*')))
    write.csv(Biomass,file.path( project.datadirectory("offshoreclams"), "R","modelBiomass.csv"),row.names=F)

    # hyperprior
    SPMpriors$r = list(a=log(SPmodel1.out$median$r.u), b=SPmodel1.out$median$r.sd,   d="dlnorm",    i1=0.2, i2=0.1, l=1   )

  ## Plotting model results

    # plot fits to abundance indices 
    SPMfit.plt(SPmodel1.out, yrs=yrs, CI=T,CV=F,graphic='pdf',H = SPMdata$Habitat, ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=420)

    # plot the posterior distributions of the estimated parameters
    SPMpost.plt(SPmodel1.out,SPMpriors, graphic='pdf',nr=2,nc=3,wd=15,name='SPM1')
    #post.plt(SPmodel.out,SPmodelpriors,years=yrs, graphic='R',nr=2,nc=3,wd=15,multi=T)


    # plot biomass 
    SPMbiomass.plt(SPmodel1.out, yrs=yrs, CI=T,graphic='pdf',ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=280)

    # exploitation
    SPMexploitation.plt(SPmodel1.out, yrs=yrs, CI=T,graphic='pdf',ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM1',ymax=0.32)


    # plot stocastic reference points
    refs=SPMRefpts(SPmodel1.out,col='grey', graphic='pdf')

    # phase plots
    SPMPhaseplts(SPmodel1.out,ymax=2.2, graphic='pdf')



#### with higher q prior

  alpha=6
  deff=.73
  #deff=.45
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

    SPmodel2.out<-runBUGS("SPhyper1", SPMdataList, SPMpriors, SPMdataList$yrs, n = 600000, burn = 100000, thin = 100,debug=F,parameters=c(names(SPMpriors),'K','P','r','B0'),sw='jags',inits=F)
    save(SPmodel1.out,file=file.path( project.datadirectory("offshoreclams"), "data", "SPM2output.Rdata" ))
    #load(file=file.path( project.datadirectory("offshoreclams"), "data", "SPM1output.Rdata" ))
    SPmodel2.out$median

    # hyperprior
    SPMpriors$r = list(a=log(SPmodel2.out$median$r.u), b=SPmodel2.out$median$r.sd,        d="dlnorm",    i1=0.2, i2=0.1, l=1   )

  ## Plotting model results

    # plot fits to abundance indices 
    SPMfit.plt(SPmodel2.out, yrs=yrs, CI=T,CV=F,graphic='R',H = SPMdata$Habitat, ht=8,wd=6,rows=5,alpha=c(0.5,0.05),name='SPM2',ymax=420)

    # plot the posterior distributions of the estimated parameters
    SPMpost.plt(SPmodel2.out,SPMpriors, graphic='R',nr=2,nc=3,wd=15,name='SPM2')
    #post.plt(SPmodel.out,SPmodelpriors,years=yrs, graphic='R',nr=2,nc=3,wd=15,multi=T)


