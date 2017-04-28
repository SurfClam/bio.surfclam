library(dplyr)
library(ggplot2)
library(htmlTable)
library(pander)
library(xtable)
library(lubridate)

source("c:/Users/StanleyR/Documents/Github/bio/bio.surfclam/inst/FunctionList.R")
#load("c:/Users/StanleyR/Documents/Github/bio.data/bio.surfclam/data/Logdata.Rdata")
load("c:/Users/StanleyR/Documents/Surf clam/Assessment/processedLogdata.Rdata")
log.data <- processed.log.data
rm(processed.log.data)
load("c:/Users/StanleyR/Documents/Surf clam/Assessment/LFdata.Rdata")

## Define thresholds for figure captions
BB_cpue <- 75
BB_footprint <- 250
BB_percentlarge <- 120
BB_quota <- "24000"

GB_cpue <- 50
GB_footprint <- 125
GB_percentlarge <- 105
GB_quota <- "14756"

#What year is it?
currentyear <- 2017

#remove any observatiosn from the current year
log.data <- log.data[log.data$year<currentyear,]


Tabdata <- CatchTable(log.data=log.data,Sel.Bank=1,tableNames = T)
Tabdata2 <- CatchTable(log.data=log.data,Sel.Bank=2,tableNames = T)

Tabdata$Bank <- "Banquereau Bank"
Tabdata2$Bank <- "Grand Bank"

Labs <- c("Catch  (t)", expression(paste("Effort (",km^2,")",sep=""))) # labels for the plots

Plotdata <- rbind(Tabdata,Tabdata2)
Plotdata <- reshape2::melt(Plotdata[,c(1:3,length(Plotdata))],id.vars=c("Year","Bank"))
Plotdata$Var=gl(2,nrow(Plotdata)/2,labels=Labs)

  limits <- data.frame(x1=c(min(Tabdata$Year,na.rm=T),1995),
                       x2=c(1995,max(Tabdata$Year,na.rm=T)),
                       y1=c(30000,24000),y2=c(30000,24000))
  limits$Bank <- "Banquereau Bank"
  limits$Var=gl(2,2,length=2,labels=Labs)

  limits2 <- data.frame(x1=c(min(CatchData$Year,na.rm=T),2010),
                      x2=c(2010,max(CatchData$Year,na.rm=T)),
                      y1=c(20000,14756),y2=c(20000,14756))
  limits2$Bank <- "Grand Bank"
  limits2$Var=gl(2,2,length=2,labels=Labs)

  plotlimits <- rbind(limits,limits2)


  p1 <- ggplot()+
    geom_line(data=Plotdata,aes(x=Year,y=value,col=Bank,group=Bank),lwd=1)+
    geom_point(data=Plotdata,aes(x=Year,y=value,col=Bank,group=Bank,fill=Bank),size=2.5,shape=21)+
    geom_segment(data=plotlimits,aes(x=x1,y=y1,xend=x2,yend=y2,col=Bank),lty=2)+
    facet_grid(Var~.,scales="free",labeller = "label_parsed")+
    theme_bw()+
    scale_x_continuous(breaks=seq(min(Plotdata$Year,na.rm=T),max(Plotdata$Year,na.rm=T),2))+
    labs(x="",y="",fill="",group="",col="")+
    theme(strip.background = element_rect(colour = "black", fill = "white"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x=element_text(angle = 45,hjust=1),
          legend.position = "bottom")+
    scale_colour_manual(values=c("grey40","black"))+
    scale_fill_manual(values=c("grey40","black"));p1

  ggsave("c:/Users/StanleyR/Documents/Surf clam/Assessment/CatchEffort-TAC.png",height = 7,width=7,p1)
