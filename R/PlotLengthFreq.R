# Length Frequency distributions.
#' @title PPlot the annual length frequency distributions.
#' @description Function to subset loci and populations
#' @param lfdata is the length frequency data from the database. Columns should include AREA (3 for Grand Bank and 4 for Banquereau)
#' YEAR, SHELL_LEN (length in mm) and NUM (frequency of this measurement in a given year).
#' @param SelBank a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank).
#' @param stat variable which defines which plot to return. "cdf" returns the cumulative length frequency distribution
#' and "lf" returns the length frequency distribution.
#' @rdname PlotLengthFreq
#' @import ggplot2
#' @importFrom dplyr ungroup mutate filter summarise
#' @importFrom lubridate year
#' @importFrom data.table data.table
#' @author Ryan Stanley
#' @export


PlotLengthFreq=function(lfdata,SelBank,stat="lf",bw=5) {

  names(lfdata) <- toupper(names(lfdata))

  lfdata$date <- strftime(lfdata$SAMPLE_DATE,format="%Y-%m-%d",tz="America/Halifax")
  lfdata$year <- year(lfdata$date)

  #Manipulate data for plotting
  expandlf <- data.table::data.table(lfdata)
  expandlf <- data.frame(expandlf[rep(seq(1,nrow(expandlf)),expandlf$NUM)])

#Plotting parameters and data filtering
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


  if(SelBank==2|SelBank==3){sizeThresh=105}
  if(SelBank==1|SelBank==4){sizeThresh=120}

  expandlf <- dplyr::filter(expandlf,RLENGTH<=200)

#Plot limits
  MaxLimit <- lfdata%>%group_by(year)%>%
                    mutate(prec=NUMBER_AT_LENGTH/sum(NUMBER_AT_LENGTH)*100)%>%
                    ungroup()%>%
                    summarise(Max=max(prec,na.rm=T))%>%as.numeric

  #Numbers of clams in each year
  Numbers <- expandlf%>%group_by(year)%>%
              summarise(num=length(NUMBER_AT_LENGTH))%>%ungroup()%>%data.frame()

  HistFun <- function(x){return(max(hist(x,bw=bw,plot=FALSE)$density*100,na.rm=T))}

  #Find cieling for the plotting functions
  MaxLimit <- plyr::round_any(x=expandlf%>%group_by(year)%>%
                                summarise(Max=HistFun(RLENGTH))%>%
                                ungroup()%>%summarise(Max=max(Max,na.rm=T))%>%as.numeric,0.5)


#Plots
  if(stat=="lf"){

    #Text locations
    Numbers$x=20;Numbers$y=MaxLimit/2

    return(ggplot()+
    geom_density(data=expandlf,aes(x=RLENGTH,y=..density..*100),bw=bw,lty=2)+
    geom_text(data=Numbers,aes(x=x,y=y,label=paste0("n=",num)))+
    facet_grid(year~.)+
    geom_vline(xintercept=sizeThresh)+
    theme_bw()+
    labs(x="Shell length (mm)",y="Frequency (%)" )+
     scale_y_continuous(breaks=c(0,round(MaxLimit)/2,round(MaxLimit)),
                         labels=c("0","",as.character(round(MaxLimit))))+
      theme(strip.background = element_rect(colour = "black", fill = "white")))
    }

  if(stat=="cdf"){

   #Text locations
    Numbers$x=20;Numbers$y=0.8

  print(ggplot()+
    stat_ecdf(data=expandlf,aes(x=RLENGTH))+
    geom_text(data=Numbers,aes(x=x,y=y,label=paste0("n=",num)))+
    facet_wrap(~year,ncol=3,scales="free_y")+
    geom_vline(xintercept=sizeThresh,lty=2)+
    theme_bw()+
    labs(x="Shell length (mm)",y="Cumlative frequency (%)" )+
      theme(strip.background = element_rect(colour = "black", fill = "white")))
    }

} #end function
