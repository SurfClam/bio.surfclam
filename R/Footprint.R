Footprint <- function(CatchData, SelBank){

  require(reshape2)
  require(ggplot2)
  
  colnames(CatchData)[2:3] <- c("Catch", "Footprint")
  Labs <- c("Catch  (t)", expression(paste("Footprint (",km^2,")",sep=""))) # labels for the plots
  CatchData <- reshape2::melt(CatchData,id.vars=c("Year","Area","CPUE"))
  
  #Banquereau
  if(SelBank==1){
    
    limits <- data.frame(x1=c(min(CatchData$Year,na.rm=T),1995,min(CatchData$Year,na.rm=T)),
                         x2=c(1995,max(CatchData$Year,na.rm=T),max(CatchData$Year,na.rm=T)),
                         y1=c(30000,24000,250),y2=c(30000,24000,250))
    
    #Set up the variables for facetting     
    limits$Var=gl(2,2,length=3,labels=Labs) #three points
    CatchData$Var=gl(2,nrow(CatchData)/2,labels=Labs) #Labels with factor levels
    
  }
  
  #Grand Bank
  if(SelBank==2){
    
    limits <- data.frame(x1=c(min(CatchData$Year,na.rm=T),2010,min(CatchData$Year,na.rm=T)),
                         x2=c(2010,max(CatchData$Year,na.rm=T),max(CatchData$Year,na.rm=T)),
                         y1=c(20000,14756,125),y2=c(20000,14756,125))

    #Set up the variables for facetting     
    limits$Var=gl(2,2,length=3,labels=Labs) #three points
    CatchData$Var=gl(2,nrow(CatchData)/2,labels=Labs) #Labels with factor levels
    
  }
  
  print(ggplot2::ggplot()+
          geom_point(data=CatchData,aes(x=Year,y=value))+
          geom_line(data=CatchData,aes(x=Year,y=value))+
          #geom_line(aes(x=Year,y=limits))+
          geom_segment(data=limits,aes(x=x1,y=y1,xend=x2,yend=y2),lty=2)+
          facet_grid(Var~.,scales="free",labeller = "label_parsed")+
          theme_bw()+
          scale_x_continuous(breaks=seq(min(CatchData$Year,na.rm=T),max(CatchData$Year,na.rm=T),3))+
          labs(x="",y="")+
          theme(strip.background = element_rect(colour = "black", fill = "white")))
  
} #end function