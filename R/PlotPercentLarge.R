PlotPercentLarge <- function(lfdata,SelBank,buffer=3,plot_loess=FALSE){
  
  #x=data.frame(year=1986:2015,prec=sample(0:10,length(1986:2015),replace=T),num=sample(804:21290,length(1986:2015))) #example data
  
  #buffer is the buffer above the maximum observed percentage
  
  require(dplyr)
  require(ggplot2)
  require(data.table)
  
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