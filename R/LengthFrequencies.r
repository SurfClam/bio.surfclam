#' @export
LengthFrequencies=function(DataList, bins=seq(0,200,1), Yrs=2005:2014, wal, fn='',... ) {

    ### Carapace Length Frequencies (LF)

    rootdir=file.path(project.datadirectory("bio.surfclam"),'figures')

    lftrips = unique(DataList$lf.data$logtrip_id)
    iLF = list()
    tripLF = list()
    WLF = list()
    tripCatch = list()
    n = c()

    for (y in 1:length(Yrs)) {

        logtrips = with(subset(DataList$log.data,year==Yrs[y]),unique(logtrip_id))
        samptrips = lftrips[lftrips%in%logtrips]
        n[y] = length(samptrips)
        iLF[[y]] = t(sapply(samptrips,function(i){with(subset(DataList$lf.data,logtrip_id==i&rlength>=min(bins)&rlength<max(bins)),hist(rep(rlength,number_at_length),breaks=bins,plot=F)$count)}))
        tripCatch[[y]] = with(subset(DataList$log.data,year==Yrs[y]&logtrip_id%in%samptrips),tapply(round_catch,logtrip_id,sum,na.rm=T))
        WLF[[y]] = rowSums(sweep(iLF[[y]],2,FUN='*',wal))
        tripLF[[y]] = sweep(iLF[[y]],1,FUN='*',tripCatch[[y]]/WLF[[y]])

    }
    
    FisheryLF = do.call("rbind",lapply(tripLF,colSums))
    
    # plot          
    BarPlotLF(list(FisheryLF),yrs=Yrs,bins=bins,col='grey',LS=NULL,sample.size=n,xlab="Shell Length (mm)",filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")), ...)
    #BubblePlotLF(FisheryLF,inch=0.2,bg=rgb(0,1,0,0.1),yrs=Yrs,bins=bins,filen=file.path(rootdir,paste0("FisheryLengthFrequency",fn,".pdf")),prop=T)
    return(FisheryLF)




}

