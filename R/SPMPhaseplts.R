#' @export
SPMPhaseplts = function(o,graphic="R",areas=1:5,nc=2,ht=7.5,wd=6,ymax=2.5,xmax=4,vline=NULL,hline=NULL,hcol=1,vcol=1,...){

	#phase plots
	o$median$B = sweep(o$median$P,MARGIN=2,o$median$K,"*")

	o$median$F = -log(1-o$data$C / o$median$B)

	colfun = colorRampPalette(brewer.pal(9,"Blues")[-1], space = "Lab")


	if(graphic=='pdf')pdf(file.path( project.datadirectory("bio.surfclam"),"figures",'PhasePlots.pdf'),height=ht,width=wd)
  	if(graphic=='png')png(filename=file.path( project.datadirectory("bio.surfclam"),"figures",'PhasePlots.png'), width=wd, height=ht, units="in", res=300)
 	if(graphic=='R')x11(height=ht,width=wd)
	
	par(mfrow=c(ceiling(length(areas)/nc),nc), mar = c(0, 0, 0, 0), omi = c(1, 1, 0.3, 0.3),las=1)

	for(i in areas) {

		smB = sRefPoints('BMSY',r=o$median$r[i],K=o$median$K[i],err=o$median$sigma)
		smF = sRefPoints('FMSY',r=o$median$r[i],K=o$median$K[i],err=o$median$sigma)

		X = o$median$B[,i] / smB
		Y = o$median$F[,i] / smF

		plot(1,1,type= 'n',xlab='',ylab='',xlim=c(0,xmax),ylim=c(0,ymax),xaxt='n',yaxt='n',...)
		if(i%%nc==1)axis(2)
		else axis(2,lab=F)
		if(i>(max(areas)-nc))axis(1)
		else axis(1,lab=F)
		#browser()
		plotArrows(X=X, Y=Y,add=T,colfun=colfun,startend=F,addpoints=F,length=0.07,lwd=2.5)
		abline(h=1,lwd=3)
		abline(v=1,lwd=3)
		if(!is.null(hline))abline(h=hline[[i]],lty=3,col=hcol,lwd=2.5)
		if(!is.null(vline))abline(v=vline[[i]],lty=3,col=vcol,lwd=2.5)
  		points(X[length(X)],Y[length(Y)],pch=21,cex=2,col='black',bg='yellow')
		text(xmax*0.9,ymax*0.9,paste('Area',i,sep="-"))
		box()

	}
   	mtext( expression(over(B,B[MSY])), 1, 5,outer=T, cex = 1.25)
   	mtext( expression(over(F,F[MSY])), 2, 3.5,outer=T, cex = 1.25)

	plot(1,1,type= 'n',axes=F)   	
	image.plot(legend.only=TRUE, col=colfun(o$data$NY), 
           zlim=range(o$data$yrs), legend.width = 3,legend.shrink=0.5,legend.mar=20)

	if(graphic!='R')dev.off()
}

