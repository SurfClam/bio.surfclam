#' @export
SPMsetup = function(vmslogdata,grid.out,vmspoly,subpolys,yrs=2003:2015,effort.min=100000,r=5,n.min=7,cv=F,cv.min=0.01,err='sd'){
  
  if(missing(subpolys)){

    # find centre points of gridded data to act as a node for estimating parameters
    useGrids = with(Totalgrid.out,subset(grid,paste(PID,SID)%in%with(subset(grid.polyData$effort[[1]],Z>effort.min),paste(PID,SID))))
    gridPoints = calcCentroid(useGrids)
    nnodes = nrow(gridPoints)

    # create circles of radius r(km) to include data around node
    subpolys = data.frame(PID=sort(rep(1:nnodes,100)),POS=rep(1:100,nnodes))
    for (i in 1:nnodes) {
      bufcircs = bufferCircle(c(gridPoints$X[i],gridPoints$Y[i]),r)
      subpolys$X[1:100+100*(i-1)] = bufcircs$lon[-101]
      subpolys$Y[1:100+100*(i-1)] = bufcircs$lat[-101]
    }
  }
  else {
    nnodes = length(unique(subpolys$PID))
  }

  # map of data points
  pdf(file.path( project.datadirectory("offshoreclams"), "figures","SPMdatapoints.pdf"),11,8)
  ClamMap2("Ban")
  addPolys(subpolys,col=rgb(1,0,0,0.1),border=rgb(0,0,0,0.1))
  with(vmslogdata,points(X,Y,pch=16,cex=0.2,col=rgb(0,0,0,.1)))
  dev.off()

  # identify data points within nodes
  key = findPolys(vmslogdata,subpolys,maxRows=1e+07)

  # assign dataset to each node
  SPdata = list()
  n = c()
  clamhabitatarea = c()
  for (i in 1:nnodes){
    cat(i)
    # calculate clam habitat
    clamhabitat = joinPolys(vmspoly,subset(subpolys,PID==i),operation="INT")
    if(!is.null(clamhabitat)){
      attr(clamhabitat,"projection") = "LL"
      clamhabitatarea[i] = calcArea(clamhabitat,1)$area # area of clam habitat with the circle

      # select data points
      tmpdata =  subset(vmslogdata,EID%in%subset(key,PID==i)$EID)
      #yrs = min(tmpdata$year):max(tmpdata$year)
      if(cv==T){
        #browser()
        tmp=jackknife(with(tmpdata,data.frame(time=year,catch=C/1000,effort=A/10^6)),err=err)
        C = tmp$catch # catch in tons
        E = tmp$effort  # effort (area swept in km2)
        O = tmp$cpue  * clamhabitatarea[i] # catch per unit effort in t / area of clam habitat
        N = tmp$n
        CV = sqrt(tmp$cpue.var)/tmp$cpue # catch per unit effort coefficient of variation
     
        n[i] = length(C)
        SPdata[[i]] = merge(data.frame(yrs=yrs,PID=i,H=clamhabitatarea[i]),data.frame(yrs=as.numeric(names(C)),C=C,E=E,O=O,CV=CV,N=N),all.x=T)
        SPdata[[i]]$C[is.na(SPdata[[i]]$C)] = 0
      }
      else{
        C = with(tmpdata,tapply(C,year,sum))/1000 # catch in tons
        E = with(tmpdata,tapply(A,year,sum)) / 10^6  # effort (area swept in km2)
        O = C / E * clamhabitatarea[i] # catch per unit effort in t / area of clam habitat
      
        n[i] = length(C)
        SPdata[[i]] = merge(data.frame(yrs=yrs,PID=i,H=clamhabitatarea[i]),data.frame(yrs=as.numeric(names(C)),C=C,E=E,O=O),all.x=T)
        SPdata[[i]]$C[is.na(SPdata[[i]]$C)] = 0
      }
    }
    else print("No habitat!")

  }

  # select only dataset with more than n.min years of data
  SPdata = SPdata[which(n>=n.min)]
  if(cv==T){
    CV=sapply(1:nnodes,function(i){SPdata[[i]]$CV})
    meanCV = mean(CV,na.rm=T)
    CV[is.na(CV)|CV==0] <- max(CV,na.rm=T)
    CV[CV<=cv.min] <- cv.min
    #browser()
    #mu=log(CV^2+1)
    model.lst=list(NJ=nnodes,NY=length(yrs),C=sapply(1:nnodes,function(i){SPdata[[i]]$C}),O=sapply(1:nnodes,function(i){SPdata[[i]]$O}),CV=CV,N=sapply(1:nnodes,function(i){SPdata[[i]]$N}))
    return(list(SPMdata=SPdata,SPMdataList=model.lst,Habitat=clamhabitatarea,meanCV=meanCV))

  }
  else {
    model.lst=list(NJ=nnodes,NY=length(yrs),C=sapply(1:nnodes,function(i){SPdata[[i]]$C}),O=sapply(1:nnodes,function(i){SPdata[[i]]$O}))
    return(list(SPMdata=SPdata,SPMdataList=model.lst,Habitat=clamhabitatarea))
  }

}
