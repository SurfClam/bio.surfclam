# Percentage of large clams.
#' @title Plot the number and percent of large clams in a sample per year with associated trigger levels.
#' @description Function to subset loci and populations
#' @param lfdata is the length frequency data from the database. Columns should include AREA (3 for Grand Bank and 4 for Banquereau)
#' YEAR, SHELL_LEN (length in mm) and NUM (frequency of this measurement in a given year).
#' @param SelBank a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank). This can also correspond to the 'bank' variable in lfdata.
#' @param buffer an integer which defines the number of percentage points above the maximum percent of large clams
#' in a given year to be added to the plot limits.
#' @param plot_loess Logical query (default = FALSE) specifying whether a loess smoothed regression should be added to the plot.
#' @rdname PlotPercentLarge
#' @import ggplot2
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom lubridate year
#' @importFrom data.table data.table
#' @author Ryan Stanley
#' @export

PlotPercentLarge <- function(lfdata,SelBank,buffer=3,plot_loess=FALSE,returnData=FALSE){

  names(lfdata) <- toupper(names(lfdata))

  #Plotting limits
  if(SelBank==3|SelBank==2){sizeThresh=105
  thresholdprec=0.5}
  if(SelBank==4|SelBank==1){sizeThresh=120
  thresholdprec=1}

  lfdata$date <- strftime(lfdata$SAMPLE_DATE,format="%Y-%m-%d",tz="America/Halifax")
  lfdata$year <- lubridate::year(lfdata$date)

  #Manipulate data for plotting
  expandlf <- data.table::data.table(lfdata)
  expandlf <- data.frame(expandlf[rep(seq(1,nrow(expandlf)),expandlf$NUMBER_AT_LENGTH)])

  #SQL query notes BB as 4 and GB as 3 which is different than the other functions. The following will account for this
  if(sum(c(lfdata$AREA==3,lfdata$AREA==4),na.rm=T)>0.6*sum(is.na(lfdata$AREA))&SelBank>2){
    expandlf <- filter(expandlf,AREA==SelBank)}

  if(sum(c(lfdata$AREA==3,lfdata$AREA==4),na.rm=T)>0.6*sum(is.na(lfdata$AREA))&SelBank<3){
    if(SelBank==1){SelBank <- 4}else{SelBank <- 3}
    expandlf <- filter(expandlf,AREA==SelBank)}

  if(sum(c(lfdata$AREA==1,lfdata$AREA==2),na.rm=T)>0.6*sum(is.na(lfdata$AREA))&SelBank<3){
    expandlf <- filter(expandlf,AREA==SelBank)}

  if(sum(c(lfdata$AREA==1,lfdata$AREA==2),na.rm=T)>0.6*sum(is.na(lfdata$AREA))&SelBank>2){
    if(SelBank==3){SelBank <- 2}else{SelBank <- 1}
    expandlf <- filter(expandlf,AREA==SelBank)}


  plotdata <- expandlf%>%filter(AREA==SelBank)%>%group_by(year)%>%
              summarise(prec=sum(RLENGTH>sizeThresh)/length(RLENGTH)*100,
                        num=length(RLENGTH))%>%
              ungroup()%>%data.frame()

  if(!plot_loess & !returnData){
  return(ggplot(plotdata,aes(x=year,y=prec,size=num))+
    geom_point()+
    scale_size("Number",range=c(1,7))+
    scale_y_continuous(limits=c(0,max(plotdata$prec,na.rm=T)+buffer))+
    geom_hline(yintercept=thresholdprec,lty=2)+
    theme_bw()+
    theme(axis.text.x=element_text(angle = 45,hjust=1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(y=bquote(paste("Percent Shell Length " >= .(sizeThresh), "mm",sep=" ")),x="")+
    scale_x_continuous(breaks=seq(min(plotdata$year,na.rm=T),max(plotdata$year,na.rm=T),2)))
    }


  if(plot_loess & !returnData){
    return(gplot(plotdata,aes(x=year,y=prec,size=num))+
      geom_point()+
      scale_size("Number",range=c(1,7))+
      scale_y_continuous(limits=c(0,max(plotdata$prec,na.rm=T)+buffer))+
      geom_hline(yintercept=thresholdprec,lty=2)+
      theme_bw()+
      theme(axis.text.x=element_text(angle = 45,hjust=1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      labs(y=bquote(paste("Percent Shell Length " >= .(sizeThresh), "mm",sep=" ")),x="")+
      scale_x_continuous(breaks=seq(min(plotdata$year,na.rm=T),max(plotdata$year,na.rm=T),2))+
      stat_smooth(se=FALSE,show.legend=FALSE,col="grey40",lwd=0.85))
      }

  if(returnData){
    plotdata$prec <- round(plotdata$prec,2)
    colnames(plotdata) <- c("Year","% large","Number unsorted")
    return(plotdata)
  }

}
