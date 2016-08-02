GridMapPlot = function(grid.out,yrs=yrs,nr=ceiling(length(yrs)/nc),nc=2,wd=8,ht=11,graphic="R",info="catch",xl=c(-60.0,-57.0),yl=c(44,45.25)){

 
	# Aspect ratio
	require(CircStats)
	aspr=1/cos(rad(mean(yl)))

  	c100 <- read.table(file.path( project.datadirectory("polygons"), "data","Basemaps","Marine","Bathymetry","CHS100.ll"),header=T)


	if(graphic=='pdf')pdf(file.path( project.datadirectory("offshoreclams"),"figures",paste0("GridMap",info,".pdf")),wd,ht)

	par(mfrow = c(nr, nc),mar=c(0,0,0,0),omi=c(0.5,0.5,0.5,0.5),las=1)
	for(i in 1:length(yrs)){
		plot(Y~X,c100,xlim=xl,ylim=yl,asp=aspr,axes=F,type='l',col=rgb(0,0,1,0.2))

		addPolys(grid.out$grid,polyProps=grid.out$grid.polyData[[info]][[i]])
		#addLines(c100,col=rgb(0,0,1,0.2))
		text(xl[1],yl[2],yrs[i],cex=2,pos=4)

		box()

	}

	if(graphic!="R")dev.off()	



}


