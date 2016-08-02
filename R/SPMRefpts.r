SPMRefpts = function(o,graphic="R",areas=1:5,nc=1,xmax=c(150000,30000,0.5),ymax=1.15,Lb=100,ht=9,wd=7,...){


	#reference points
	BMSY=c()
	MSY=c()
	FMSY=c()


	if(graphic=='pdf'){
		pdf(file.path( project.datadirectory("offshoreclams"),"figures",'SurfClamRefs.pdf'),height=ht,width=wd)
		par(mfrow=c(ceiling(length(areas)/nc),nc), mar = c(0, 3, 0, 1), omi = c(0.7, 0.3, 0.3, 0.3),las=1)
	}
 	if(graphic=='R'){
		x11(height=ht,width=wd)
		par(mfrow=c(ceiling(length(areas)/nc),nc), mar = c(0, 3, 0, 1), omi = c(0.7, 0.3, 0.3, 0.3),las=1)
 	}
 	for(i in areas)	{

		smB = sRefPoints('BMSY',r=o$sims.list$r[,i],K=o$sims.list$K[,i],err=o$sims.list$sigma)
		mB = sRefPoints('BMSY',r=o$sims.list$r[,i],K=o$sims.list$K[,i],err=0)
		tmp = hist(mB[mB<xmax[1]],breaks=seq(0,xmax[1],l=Lb),plot=F)
		tmp$counts = tmp$counts/max(tmp$counts)
		plot(tmp,ylab='',xlim=c(0,xmax[1]),ylim=c(0,ymax),main="",xlab="",...)
		abline(v=median(smB),col='red',lwd=2)
		abline(v=median(mB),col='blue',lwd=2)
		 legend('topright',lty=c(1,1),col=c('red','blue'),c(paste('stoch. Median=',round(median(smB),0),sep=""),paste('Median=',round(median(mB),0),sep="")),bty='n',title=paste('Area',i,sep="-"),inset=c(0,0.2))
     	mtext( expression(B[MSY]), 1, 3,outer=T, cex = 1.25)
     	BMSY[i]=median(smB)
		}

 	if(graphic=='R'){
		x11(height=ht,width=wd)
		par(mfrow=c(ceiling(length(areas)/nc),nc), mar = c(0, 3, 0, 1), omi = c(0.7, 0.3, 0.3, 0.3),las=1)
 	}

 	for(i in areas)	{
		smB = sRefPoints('MSY',r=o$sims.list$r[,i],K=o$sims.list$K[,i],err=o$sims.list$sigma)
		mB = sRefPoints('MSY',r=o$sims.list$r[,i],K=o$sims.list$K[,i],err=0)
		tmp = hist(mB[mB<xmax[2]],breaks=seq(0,xmax[2],l=Lb),plot=F)
		tmp$counts = tmp$counts/max(tmp$counts)
		plot(tmp,ylab='',xlim=c(0,xmax[2]),ylim=c(0,ymax),main="",xlab="",...)
		abline(v=median(smB),col='red',lwd=2)
		abline(v=median(mB),col='blue',lwd=2)
		legend('topright',lty=c(1,1),col=c('red','blue'),c(paste('stoch. Median=',round(median(smB),0),sep=""),paste('Median=',round(median(mB),0),sep="")),bty='n',title=paste('Area',i,sep="-"),inset=c(0,0.2))
     	mtext( "MSY", 1, 3,outer=T, cex = 1.25)
     	MSY[i]=median(smB)
	}

  	if(graphic=='R'){
		x11(height=ht,width=wd)
		par(mfrow=c(ceiling(length(areas)/nc),nc), mar = c(0, 3, 0, 1), omi = c(0.7, 0.3, 0.3, 0.3),las=1)
 	}
	for(i in areas)	{
		smB = sRefPoints('FMSY',r=o$sims.list$r[,i],K=o$sims.list$K[,i],err=o$sims.list$sigma)
		mB = sRefPoints('FMSY',r=o$sims.list$r[,i],K=o$sims.list$K[,i],err=0)
		tmp = hist(mB[mB<xmax[3]],breaks=seq(0,xmax[3],l=Lb),plot=F)
		tmp$counts = tmp$counts/max(tmp$counts)
		plot(tmp,ylab='',xlim=c(0,xmax[3]),ylim=c(0,ymax),main="",xlab="",...)
		abline(v=median(smB),col='red',lwd=2)
		abline(v=median(mB),col='blue',lwd=2)
		legend('topright',lty=c(1,1),col=c('red','blue'),c(paste('stoch. Median=',round(median(smB),2),sep=""),paste('Median=',round(median(mB),2),sep="")),bty='n',title=paste('Area',i,sep="-"),inset=c(0,0.2))
     	mtext( expression(F[MSY]), 1, 3,outer=T, cex = 1.25)
     	FMSY[i]=median(smB)

	
	}
	if(graphic!='R')dev.off()

	return(list(BMSY=BMSY,MSY=MSY,FMSY=FMSY))
}








