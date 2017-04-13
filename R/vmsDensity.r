#' @export
vmsDensity = function(vmslogdata,sig=0.1,res=0.25,lvl=30,lab='',zone=21){

	require(spatstat)
    
  attr(vmslogdata,'projection')<-"LL"
  vmslogdata = convUL(vmslogdata)
  vmsDensity <- with(vmslogdata,ppp(X, Y, range(X), range(Y)))

  x = range(vmslogdata$X)
  y = range(vmslogdata$Y)

  rect = data.frame(PID=c(1,1,2,2),POS=c(1,2,1,2),X=c(x[1],x[2],rep(mean(x),2)),Y=c(rep(mean(y),2),y[1],y[2]))
  attr(rect,'projection')<-"UTM"
  lens = round(calcLength(rect)$length)
  spatstat.options(npixel=round(lens/res))
  #plot(density(vmsDensity,0.001))

  x = density(vmsDensity,sig)
  plot(x,main=paste("VMS Density, sig=",sig,"res=",res,"lvl=",lvl))
  contour(x,levels=lvl,drawlabels=F,add=T)
 
  x.lst = list(x=seq(x$xrange[1]+x$xstep/2,x$xrange[2],x$xstep),y=seq(x$yrange[1]+x$ystep/2,x$yrange[2],x$ystep),z=t(x$v))
  save(x.lst,file=file.path( project.datadirectory("bio.surfclam"),'data',paste0(lab,'VMSdensity.rdata')))

  CL <- contourLines(x.lst,levels=lvl)
  CP <- convCP(CL)
  VMSden.poly <- CP$PolySet

  attr(VMSden.poly,'projection')<-"UTM"
  attr(VMSden.poly,'zone')<-zone
  VMSden.poly = convUL(VMSden.poly)

  junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
  VMSden.poly = joinPolys(VMSden.poly,junk,operation="DIFF")


  return(VMSden.poly)
}