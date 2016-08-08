#' @export
SPMbiomass.plt <- function(model.out,yrs,name="", rows=3, CI=F,graphic='R',ymax,alpha=0.05,refs=NULL,refcol=rgb(0,1,0,0.8),path=file.path( project.datadirectory("bio.surfclam"), "figures"),ht=8,wd=6){

  # SPMbiomass plots
  
  
  if(graphic=='pdf')pdf(file.path(path,paste(name, "biomass.pdf", sep="")),height=ht,width=wd)
  if(graphic=="R")x11(ht,wd)
  
  if(missing(yrs))yrs = 1:model.out$data$NY
  NJ = model.out$data$NJ


  par(mfrow = c(rows,1), mar = c(0, 3, 0, 1), omi = c(0.5, 0.3, 0.3, 0.3))
  
  for(j in 1:NJ){
  
  
    Bposts = sweep(model.out$sims.list$P[,,j],1,FUN='*',model.out$sims.list$K[,j]/1000)
    Bmed = model.out$median$P[,j]*model.out$median$K[j]/1000
   
    if(missing(ymax))yl2<-ifelse(CI,max(apply(Bposts, 2, quantile, 1-min(alpha)/2),na.rm=T),max(Bmed,na.rm=T))*1.1
    else yl2 = ymax
    #browser()
    plot(yrs, (Bmed[1:length(yrs)]), type = 'l', lwd = 2, ylim = c(0, yl2),   ylab = "", las = 1, xlim = c(min(yrs)-1, max(yrs)+1), mgp = c(0.5, 0.5, 0), xlab = "", tcl = -  0.3, asp = 'xy', cex.axis=1.2,xaxt='n')
    axis(1, lab = F, tcl = -0.3)
    axis(4, lab = F, tcl = -0.3)
    if(CI){
      CIyrs=matrix(yrs,length(alpha),length(yrs),byrow=T)
      CIl=apply(Bposts, 2, quantile, alpha/2)
      CIu=apply(Bposts, 2, quantile, 1-alpha/2)
      for(i in 1:nrow(CIyrs)){
	    lines(CIyrs[i,], CIl[i,], lty = i+1)
	    lines(CIyrs[i,], CIu[i,], lty = i+1)
	  }
    }
    if(!is.null(refs))abline(h=refs[j],col=refcol)
    text(yrs[length(yrs)]+1,yl2,j,pos=1,cex=1.5)
   
 
    if(j%in%(rows*(1:floor(NJ/rows)))){
      axis(1)
      mtext( "Fishable Biomass (kt)", 2, 0,outer=T, cex = 1.25)
      if(graphic=="R"&&j != NJ){
      	x11(ht,wd)
        par(mfrow = c(rows,1), mar = c(0, 3, 0, 1), omi = c(0.5, 0.3, 0.3, 0.3))
      }

    }
    if(j == NJ){
      axis(1)
      mtext( "Fishable Biomass (kt)", 2, 0,outer=T, cex = 1.25)
    }
  }
  if(graphic!="R")dev.off()
}

