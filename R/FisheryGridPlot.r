#' @export
FisheryGridPlot <- function(fisheryList, p, boundPoly, vms=FALSE, fn='',cpue=TRUE, aspr='calculate', lg.place="bottomright",effort.units='ha',wd=11,ht=8,outsideBorder=F,...){

  if(effort.units=='ha')efadj=10^4
  if(effort.units=='km2')efadj=10^6
  if(effort.units=='m2')efadj=1

  ## Grid Plots
  b=ifelse(p$bank=="Ban",1,2)
  yrs=p$yrs

  recordgrids=list()
  effortgrids=list()
  catchgrids=list()
  grid.polyData=list()

  # setup data to summarize by grid
  if(vms) {
    logdata = assignLogData2VMS(fisheryList, p)
    logdata = logdata[,c("year","EID","X","Y","C","A")]
  }
  else {
    logdata = na.omit(subset(fisheryList$log.data, bank==b&area>p$effort.threshold[1]&area<p$effort.threshold[2]&round_catch>p$catch.threshold[1]&round_catch<p$catch.threshold[2],  c("year","logrecord_id","lon_dd","lat_dd","round_catch","area")))
  }
  names(logdata)<-c("year","EID","X","Y","C","A")
  logdata$R=1

  # boundPoly
  if(!missing(boundPoly))logdata = subset(logdata,EID%in%findPolys(logdata,boundPoly, maxRows = 1e+06)$EID)

   # EFFORT

    grid.polyData[[1]]<-list()
    pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(fn,p$bank,"Effort.pdf")),width=wd,height=ht)

     
     for(y in 1:length(yrs)){

      grid.dat=subset( logdata ,year%in%yrs[[y]],c("EID","X","Y","A"))
      print(paste(y,Sys.time()))
      print(summary(grid.dat))
      #browser()
      if(nrow(grid.dat)>0){
       
        effortgrids[[y]]<-gridData(grid.dat,lvls=p$effort.levels,bcol=p$catch.cols,FUN=sum,border=NA,grid.size=p$grid.size,aspr=aspr,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)
        grid.polyData[[1]][[y]] = effortgrids[[y]][[2]]
        
        titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))
       
        ClamMap2(p$bank,poly.lst=effortgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Effort"),...)
        ContLegend(lg.place,lvls=effortgrids[[y]]$lvls/efadj,Cont.data=effortgrids[[y]],title=paste0("Area Fished (",effort.units,")"),inset=0.02,cex=0.8,bg='white')

        if(outsideBorder){
          combinedgrid=subset(effortgrids[[y]][[1]],paste(PID,SID)%in%with(effortgrids[[y]][[2]],paste(PID,SID)))
          addPolys(joinPolys(combinedgrid,operation="UNION"),border=rgb(0,0,0,0.5))
        }


       }
     
     }

    dev.off()


    # CATCH

    grid.polyData[[2]]<-list()
    pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(fn,p$bank,"Catch.pdf")),width=wd,height=ht)
     
     for(y in 1:length(yrs)){
     
      grid.dat=subset( logdata ,year%in%yrs[[y]],c("EID","X","Y","C"))
      print(paste(y,Sys.time()))
      print(summary(grid.dat))
      if(nrow(grid.dat)>0){
       
       catchgrids[[y]]<-gridData(grid.dat,lvls=p$catch.levels,bcol=p$catch.cols,FUN=sum,border=NA,grid.size=p$grid.size,aspr=aspr,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)
       grid.polyData[[2]][[y]] = catchgrids[[y]][[2]]
       
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))

       ClamMap2(p$bank,poly.lst=catchgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Catch"),...)
       ContLegend(lg.place,lvls=catchgrids[[y]]$lvls/10^3,Cont.data=catchgrids[[y]],title="Catch (t)",inset=0.02,cex=0.8,bg='white')
       
       if(outsideBorder){
        combinedgrid=subset(catchgrids[[y]][[1]],paste(PID,SID)%in%with(catchgrids[[y]][[2]],paste(PID,SID)))
        addPolys(joinPolys(combinedgrid,operation="UNION"),border=rgb(0,0,0,0.5),lwd=0.1)
      }


       }
     
     }

    dev.off()


    # CPUE
    grid.polyData[[3]]<-list()
    cpuegrids = catchgrids

    pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(fn,p$bank,"CPUE.pdf")),width=wd,height=ht)
     
     for(y in which(!unlist(lapply(effortgrids,is.null)))){
      print(paste(y,Sys.time()))

        cpuegrids[[y]][[2]]$Z <- catchgrids[[y]][[2]]$Z / effortgrids[[y]][[2]]$Z
        cpuegrids[[y]][[2]]$Z[is.infinite(cpuegrids[[y]][[2]]$Z)] <- NA
        cpuegrids[[y]][[2]]$Z[cpuegrids[[y]][[2]]$Z==0] <- NA
        cpuegrids[[y]][[2]]$Z[effortgrids[[y]][[2]]$Z<p$cpue.threshold[1]] <- NA
      
        cols   <- brewer.pal(length(p$cpue.levels),p$cpue.cols) 
        pdata  <- makeProps(na.omit(cpuegrids[[y]][[2]][,1:3]), c(p$cpue.levels,max(p$cpue.levels)*100), "col", cols) 
        pdata$border  <- NA
        cpuegrids[[y]][[2]] <- pdata
       
       grid.polyData[[3]][[y]] = cpuegrids[[y]][[2]]
       
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))

       ClamMap2(p$bank,poly.lst=cpuegrids[[y]][1:2],title=paste(titleyr,"Surf Clam CPUE"),...)
       ContLegend(lg.place,lvls=p$cpue.levels*1000,Cont.data=cpuegrids[[y]],title=expression(CPUE (t/km^2)),inset=0.02,cex=0.8,bg='white')
       
       if(outsideBorder){
        combinedgrid=subset(cpuegrids[[y]][[1]],paste(PID,SID)%in%with(cpuegrids[[y]][[2]],paste(PID,SID)))
        addPolys(joinPolys(combinedgrid,operation="UNION"),border=rgb(0,0,0,0.5))
      }


       }

    dev.off()


  # EXPLOITATION
    pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(fn,p$bank,"Exploitation.pdf")),width=wd,height=ht)
     
     for(y in which(!unlist(lapply(effortgrids,is.null)))){
 
       titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))
        
       ClamMap2(p$bank,poly.lst=effortgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Exploitation"),...)
       ContLegend(lg.place,lvls=p$effort.levels/10^6/p$grid.size^2,Cont.data=effortgrids[[y]],title="Exploitation Rate",inset=0.02,cex=0.8,bg='white')
      
       if(outsideBorder){
        combinedgrid=subset(effortgrids[[y]][[1]],paste(PID,SID)%in%with(effortgrids[[y]][[2]],paste(PID,SID)))
        addPolys(joinPolys(combinedgrid,operation="UNION"),border=rgb(0,0,0,0.5))
      }


       }

    dev.off()

    # RECORD DENSITY

    grid.polyData[[4]]<-list()
    pdf(file.path( project.datadirectory("bio.surfclam"), "figures",paste0(fn,p$bank,"Records.pdf")),width=wd,height=ht)

     
     for(y in 1:length(yrs)){

      grid.dat=subset( logdata ,year%in%yrs[[y]],c("EID","X","Y","R"))
      print(paste(y,Sys.time()))
      print(summary(grid.dat))
      #browser()
      if(nrow(grid.dat)>0){
       
        recordgrids[[y]]<-gridData(grid.dat,lvls=p$record.levels,bcol=p$catch.cols,FUN=sum,border=NA,grid.size=p$grid.size,aspr=aspr,sx=p$Min_lon,sy=p$Min_lat,ex=p$Max_lon,ey=p$Max_lat)
        grid.polyData[[4]][[y]] = recordgrids[[y]][[2]]
        
        titleyr = ifelse(length(yrs[[y]])==1,yrs[[y]],paste(min(yrs[[y]]),max(yrs[[y]]),sep='-'))
       
        ClamMap2(p$bank,poly.lst=recordgrids[[y]][1:2],title=paste(titleyr,"Surf Clam Records"),...)
        ContLegend(lg.place,lvls=recordgrids[[y]]$lvls,Cont.data=recordgrids[[y]],title="Records",inset=0.02,cex=0.8,bg='white')

        if(outsideBorder){
          combinedgrid=subset(recordgrids[[y]][[1]],paste(PID,SID)%in%with(recordgrids[[y]][[2]],paste(PID,SID)))
          addPolys(joinPolys(combinedgrid,operation="UNION"),border=rgb(0,0,0,0.5))
        }

       }
     
     }

    dev.off()

 
    names(grid.polyData[[1]]) = yrs
    names(grid.polyData[[2]]) = yrs
    names(grid.polyData[[3]]) = yrs
    names(grid.polyData[[4]]) = yrs
    names(grid.polyData) = c("effort","catch","cpue","records")

     return(list(grid=catchgrids[[length(yrs)]][[1]], grid.polyData=grid.polyData))
  }

