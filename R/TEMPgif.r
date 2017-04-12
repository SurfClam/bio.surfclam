#' @export
TEMPgif<-function(fisheryList,yrs,interval=1,pause=0.05,tail=7,pie.scale=10,wd=800,ht=600,ptcex=0.1,ptcol=rgb(1,0,0,0.3),out.dir=file.path( project.datadirectory("offshoreclams"), "figures"),...){
	
	require(animation)# for creating gif
	require(TeachingDemos) # for adding pie charts to map
	

	# VMS data
	VMSdat = fisheryList$vms.data
	if(missing(yrs))yrs = unique(VMSdat$year)
	VMSdat = subset(VMSdat, year%in%yrs)
	VMSdat$date =	as.Date(VMSdat$date)

	VMSdat$julian<-julian(VMSdat$date,origin=min(VMSdat$date)-1)
	
	# log data
	fish.dat = fisheryList$log.data
	fishTmp.dat<-subset(fish.dat,year%in%yrs)
	fishTmp.dat$julian<-julian(as.Date(fishTmp.dat$date),origin=min(VMSdat$date)-1)
	BanSum<-sum(subset(fishTmp.dat,bank==1,round_catch))
	BanS<-sqrt(BanSum/pi)/pie.scale
	

	### GIF animations ###
	## set some options first
	oopt = ani.options(interval = 0.4, nmax = length(min(VMSdat$julian):max(VMSdat$julian)), outdir=out.dir)
	## use a loop to create images one by one
	saveGIF({
	for (i in seq(1,ani.options("nmax"),interval)) {
	ClamMap2(...,title=min(VMSdat$date)+i-1)
	points(lat~lon,subset(VMSdat,julian<=i&julian>i-tail),pch=16,cex=ptcex,col=ptcol) # add VMS
	 print(i)
	# Catch pie charts
	BanFSF<-sum(subset(fishTmp.dat,julian<i&bank==1)$round_catch)
	#browser()
	subplot(pie(c(BanSum-BanFSF,BanFSF),labels=NA),-57.5,44.3,size=rep(BanS,2))

	if(i == ani.options("nmax"))	 points(lat~lon,VMSdat,pch=16,cex=ptcex,col=) # add VMS
 
	 
	ani.pause() ## pause for a while (’interval’)
	}
	}, interval = pause, movie.name = "VMS.gif", ani.width = wd, ani.height = ht)
}


