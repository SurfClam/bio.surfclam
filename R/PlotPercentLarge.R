# Percentage of large clams.
#' @title Plot the number and percent of large clams in a sample per year with associated trigger levels.
#' @description Function to subset loci and populations
#' @param lfdata is the length frequency data from the database. Columns should include AREA (3 for Grand Bank and 4 for Banquereau)
#' YEAR, SHELL_LEN (length in mm) and NUM (frequency of this measurement in a given year).
#' @param SelBank a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank).
#' @param buffer an integer which defines the number of percentage points above the maximum percent of large clams
#' in a given year to be added to the plot limits.
#' @param plot_loess Logical query (default = FALSE) specifying whether a loess smoothed regression should be added to the plot.
#' @rdname PlotPercentLarge
#' @import ggplot2
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom data.table data.table
#' @export

PlotPercentLarge <- function(lfdata,SelBank,buffer=3,plot_loess=FALSE){

  #Plotting limits
  if(SelBank==3){sizeThresh=105
  thresholdprec=0.5}
  if(SelBank==4){sizeThresh=120
  thresholdprec=1}

  #Manipulate data for plotting
  expandlf <- data.table::data.table(lfdata)
  expandlf <- data.frame(expandlf[rep(seq(1,nrow(expandlf)),expandlf$NUM)])
  expandlf <- filter(expandlf,AREA==SelBank)

  plotdata <- expandlf%>%filter(AREA==SelBank)%>%group_by(YEAR)%>%
              summarise(prec=sum(SHELL_LEN>sizeThresh)/length(SHELL_LEN)*100,
                        num=length(SHELL_LEN))%>%
              ungroup()%>%data.frame()

  p1 <- ggplot(plotdata,aes(x=YEAR,y=prec,size=num))+
    geom_point()+
    scale_size("Number",range=c(1,7))+
    scale_y_continuous(limits=c(0,max(plotdata$prec,na.rm=T)+buffer))+
    geom_hline(yintercept=thresholdprec,lty=2)+
    theme_bw()+
    labs(y=bquote(paste("Percent Shell Length " >= .(sizeThresh), "mm",sep=" ")),x="")+
    scale_x_continuous(breaks=seq(min(plotdata$YEAR,na.rm=T),max(plotdata$YEAR,na.rm=T),3))




  if(plot_loess){p1 <- p1+stat_smooth(se=FALSE,show.legend=FALSE,col="grey40",lwd=0.85)}

  print(p1)

}
