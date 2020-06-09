#' @export
SeasonalCPUE<-function(vmslogdata,yrs,subpolys,lab='',graphic='R',wd=8,ht=11,err='sd',effort.min=0.015,annual.pts=T,...){

	
	if(missing(yrs))yrs<-unique(vmslogdata$year)

	areas = unique(subpolys$PID)
  	key = findPolys(vmslogdata,subpolys,maxRows=1e+07)

	vmslogdata$vmsdatelocal<-as.Date(vmslogdata$vmsdatelocal)
	dates<-data.frame(DATE=seq(min(vmslogdata$vmsdatelocal),max(vmslogdata$vmsdatelocal),1))
	daily<-list()
	annual<-list()
	for(i in 1:length(areas)){
	
		tmpdata =  subset(vmslogdata,EID%in%subset(key,PID==i)$EID)
		
		# daily
		catch<-with(tmpdata,tapply(C,vmsdatelocal,sum,na.rm=T))/1000
		effort<-with(tmpdata,tapply(A,vmsdatelocal,sum,na.rm=T))/10^6
		daily[[i]]<-merge(data.frame(AREA=areas[i],DATE=as.Date(names(catch)),CATCH=catch),data.frame(AREA=areas[i],DATE=as.Date(names(effort)),EFFORT=effort),all=T)
		daily[[i]]$CPUE<-daily[[i]]$CATCH/daily[[i]]$EFFORT

		# annual
		if(annual.pts==T)day<-as.Date(paste0(sort(unique(tmpdata$year)),"-01-01"))
		if(annual.pts==F)day<-with(tmpdata,tapply(vmsdatelocal,year,mean,na.rm=T))
		tmp=jackknife(with(tmpdata,data.frame(time=year,catch=C/1000,effort=A/10^6)),err=err)
		annual[[i]]<-data.frame(AREA=areas[i],DATE=day,CATCH=tmp$catch,EFFORT=tmp$effort,CPUE=tmp$cpue,CV = sqrt(tmp$cpue.var)/tmp$cpue)


	}
	daily.dat<-do.call("rbind",daily)
	daily.dat<-subset(daily.dat,EFFORT>effort.min)
	daily.dat<-merge(daily.dat,merge(dates,data.frame(AREA=areas)),all=T)
	annual.dat<-do.call("rbind",annual)

	if(graphic=='pdf')pdf(file.path( project.datadirectory("bio.surfclam"),"figures",paste0("CPUE",lab,".pdf")), width=wd, height=ht)
	if(graphic=='png')png(file.path( project.datadirectory("bio.surfclam"),"figures",paste0("CPUE",lab,".png")), width=wd, height=ht, units="in", res=300)

	par(mfrow=c(length(areas),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)

	for(i in 1:length(areas)){

		plot(CPUE~DATE,subset(daily.dat,AREA==areas[i]),ylim=c(0,max(daily.dat$CPUE,na.rm=T)),...)
		with(subset(annual.dat,AREA==areas[i]),segments(DATE, CPUE+CV*CPUE, DATE, CPUE-CV*CPUE,col='red'))
		lines(CPUE~DATE,subset(annual.dat,AREA==areas[i]),type='b',pch=21,bg='red')
		text(min(daily.dat$DATE,na.rm=T),max(daily.dat$CPUE,na.rm=T)*.7,paste("AREA",areas[i]),cex=2,pos=4)
	}
	mtext(expression(CPUE(t/km^2)), 2, 3, outer = T, cex = 1.5,las=0)	

	if(graphic!='R')dev.off()

	return(list(Daily=daily.dat,Annual=annual.dat))
}
