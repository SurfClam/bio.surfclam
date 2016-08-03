#' @export
CatchEffortPlot = function(CatchEffortData,yrs,graphic='R',col=rgb(0,1,0,0.8),ht=8,wd=6,path=file.path( project.datadirectory("offshoreclams"), "figures")){

  # CatchEffortPlots
  
  
  if(graphic=='pdf')pdf(file.path(path,"CatchEffortPlot.pdf"),height=ht,width=wd)
  if(graphic=="R")x11(ht,wd)
  
  if(missing(yrs))yrs = CatchEffortData$Year
  d = subset(CatchEffortData,Year%in%yrs)


  par(mfrow = c(2,1), mar = c(0, 4, 0, 1), omi = c(0.5, 0.3, 0.3, 0.3),las=1)
  
   
  
   #browser()
    plot(yrs, d$Grand.Catch/1000, type = 'b', lwd = 2, ylim = c(0, max(d$Ban.Catch/1000,na.rm=T)), col='grey50', pch=17,cex.axis=1.2, ylab = "", xlab = "",   xaxt='n')
    lines(yrs, d$Ban.Catch/1000, type = 'b', lwd = 2,pch=16)
    axis(1, lab = F)
    mtext("Catch (kt)",2,3,las=0,cex=1.2)
   
    legend('right',c("Banqureau","Grand Bank"),pch=16:17,col=c('black','grey50'),cex=1,bty='n')
    
    plot(yrs, d$Grand.Effort, type = 'b', lwd = 2, ylim = c(0, max(d$Ban.Effort,na.rm=T)), col='grey50', pch=17, ylab = "",cex.axis=1.2, xlab = "", xaxt='n')
    lines(yrs, d$Ban.Effort, type = 'b', lwd = 2,pch=16)
    axis(1,cex.axis=1.2)
    mtext(expression(Effort ( km^2 )),2,3,las=0,cex=1.2)
  
    
   if(graphic!="R")dev.off()
}

