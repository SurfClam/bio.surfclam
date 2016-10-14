# Catch per unit effort plot.
#' @title Plot the annual CPUE and associated trigger level.
#' @description Function to subset loci and populations
#' @param logdata is the processed log data from the functions GetLogData() and ProcessLogData().
#' @param SelBank a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank).
#' @param stat variable (default="mean") defining whether the plot returned should have all the ship specific means (stat="all")
#' in a given year or should represented as the average for a ship in a given year.
#' Note if variable anon = TRUE only the grouped means will be returned.
#' @param returnData (default=FALSE) if true the function will not return a plot but will instead return a
#' dataframe summarizing the yearly averaged landings.
#' @param anon (default=FALSE) should the output be anonymized for public consumption (no ship specific data).
#' @rdname CpuePlot
#' @import ggplot2
#' @importFrom lubridate year
#' @importFrom dplyr summarise mutate ungroup filter
#' @export

CpuePlot <- function(logdata, SelBank,stat="mean",returnData=FALSE,anon=FALSE){

  #CPUE trigger
  if(SelBank==1){trigger=75}
  if(SelBank==2){trigger=50}

  ## matrix of info for last four vessles active in fleet (Note this is hard coded)
  PlotParameters<- data.frame(CFV=c(101277, 101276, 133542, 176085, 000000),
                              VN=c("Atlantic Vigour", "Atlantic Pursuit", "Ocean Concord",
                                   "Arctic Endurance", "Fleet average"),
                              Col=c("red", "blue", "green", "orange", "black"),stringsAsFactors = F)

  ## Data for last 4 vessels
  PlotSub <-  dplyr::filter(logdata,CFV %in% PlotParameters$CFV,BANK == SelBank)%>%
    dplyr::group_by(CFV,TRIP_NO)%>%
    dplyr::summarise(RECORD_DATE=mean(RECORD_DATE,na.rm=T),
                     ROUND_CATCH=sum(ROUND_CATCH,na.rm=T),
                     AREA=sum(AREA,na.rm=T))%>%ungroup()%>%data.frame()%>%
    dplyr::mutate(.,CPUE=ROUND_CATCH/AREA*1000,
                  Year=lubridate::year(RECORD_DATE))

  PlotSub <- merge(PlotSub,PlotParameters,by="CFV")

  PlotAggYear <- PlotSub%>%dplyr::group_by(Year,CFV)%>%
    summarise(se=sd(CPUE,na.rm=T)/sqrt(sum(!is.na(CPUE))),
              sd=sd(CPUE,na.rm=T),
              CPUE=mean(CPUE,na.rm=T))%>%
    ungroup()%>%data.frame()

  PlotAgg <- PlotSub%>%dplyr::group_by(Year)%>%
    summarise(se=sd(CPUE,na.rm=T)/sqrt(sum(!is.na(CPUE))),
              sd=sd(CPUE,na.rm=T),
              CPUE=mean(CPUE,na.rm=T))%>%
    ungroup()%>%data.frame()

  PlotAgg$CFV=000000
  PlotAgg=rbind(PlotAggYear,PlotAgg[,colnames(PlotAggYear)])
  PlotAgg <- merge(PlotAgg,PlotParameters,by="CFV")
  PlotAgg[is.na(PlotAgg)]=NA #no NAs
  #Set plot levels
  PlotSub=rbind(PlotSub,rep(NA,length(PlotSub)))
  PlotSub[nrow(PlotSub),"VN"]="Fleet average"#Dummy for ggplot labels
  PlotSub$VN <- factor(PlotSub$VN,
                       levels=PlotParameters$VN)
  PlotAgg$VN <- factor(PlotAgg$VN,levels=PlotParameters$VN)

  #Plot parameters
  dodge <- position_dodge(.2)

  #Plot - all non-anonymous
  if(stat=="all" & !returnData & !anon){
    print(ggplot()+
            geom_point(data=PlotSub,aes(x=Year,y=CPUE,col=VN),position=dodge)+
            geom_line(data=PlotAgg[PlotAgg$VN=="Fleet average",],aes(x=Year,y=CPUE,col=VN))+
            geom_hline(yintercept=trigger,lty=2)+
            theme_bw()+theme(legend.position="bottom")+
            labs(x="",y=expression(paste("CPUE ",g/m^2,sep="")),col="")+
            scale_colour_manual(values = PlotParameters$Col))
  }

  if(stat=="means" & !returnData & !anon){

    print(ggplot()+
            geom_line(data=PlotAgg[PlotAgg$VN=="Fleet average",],
                      aes(x=Year,y=CPUE),lty=1,lwd=1.1)+
            geom_errorbar(data=PlotAgg[PlotAgg$VN!="Fleet average",],
                          aes(x=Year,y=CPUE,col=VN,ymin=CPUE-se,ymax=CPUE+se),
                          width=0.2,position = dodge)+
            geom_point(data=PlotAgg[PlotAgg$VN!="Fleet average",],aes(x=Year,y=CPUE,col=VN),
                       position = dodge,size=1.85)+
            geom_hline(aes(yintercept=trigger),lty=2)+
            theme_bw()+
            labs(x="",y=expression(paste("CPUE ",g/m^2 %+-% " se",sep="")),col="")+
            scale_colour_brewer(palette = "Set1")+
            theme(legend.position="bottom",axis.text.x=element_text(angle = 45,hjust=1))+
            scale_x_continuous(breaks=seq(min(PlotAgg$Year,na.rm=T),
                                          max(PlotAgg$Year,na.rm=T),3)))

  }



  #Anonymous report

  #Plot - all non-anonymous

  if(!returnData & anon){

    print(ggplot()+
            geom_line(data=PlotAgg[PlotAgg$VN=="Fleet average",],
                      aes(x=Year,y=CPUE),lty=1,lwd=0.6)+
            geom_errorbar(data=PlotAgg[PlotAgg$VN=="Fleet average",],
                          aes(x=Year,y=CPUE,ymin=CPUE-se,ymax=CPUE+se),
                          width=0.2,position = dodge)+
            geom_point(data=PlotAgg[PlotAgg$VN=="Fleet average",],
                       aes(x=Year,y=CPUE),
                       position = dodge,size=1.85)+
            geom_hline(aes(yintercept=trigger),lty=2)+
            theme_bw()+
            labs(x="",y=expression(paste("CPUE ",g/m^2 %+-% " se",sep="")),col="")+
            scale_colour_brewer(palette = "Set1")+
            theme(legend.position="none",
                  axis.text.x=element_text(angle = 45,hjust=1))+
            scale_x_continuous(breaks=seq(min(PlotAgg$Year,na.rm=T),
                                          max(PlotAgg$Year,na.rm=T),3)))

  }

  #Return the fleet average data
  if(returnData & !anon){
    return(droplevels(PlotAgg[PlotAgg$VN=="Fleet average",
                              c("CFV","Year","CPUE","sd","se")]))}

}
