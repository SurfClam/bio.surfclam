# One minute squared fished per year.
#' @title Plot the fishing effort as a function of one minute squared 'cells' fished annually.
#' @description Function to subset loci and populations
#' @param logdata is the processed log data from the functions GetLogData() and ProcessLogData().
#' @param SelBank a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank).
#' @rdname OMSfished
#' @import ggplot2
#' @author Brad Hubley and Ryan Stanley
#' @export

OMSfished <- function(logdata, SelBank=NA,returnData=FALSE){

  require(ggplot2)

  if(is.na(SelBank)){stop("Bank not specified. Please define 'SelBank' as either 1 (Banquereau) or 2 (Grand Bank). Function stopped")}

  logdata <- logdata[logdata$bank==SelBank,]

#Create data for plot (from original code)
  # Lat and Long with positve Long
    pos <- cbind(logdata$lat_dd, -1  *logdata$lon_dd)
  # one minute resolution DDMM
    squares <- trunc(pos) * 100 + (1 + trunc((pos %% 1) * 60))
  # DDMMDDMM as one value for unique square
    oms <- squares[ ,1] * 10000 + squares[ ,2]
  # number of unique 1 minute squares fished
    squares.fished <- length(unique(oms))
  # oms.freq is a table of frequency of minute squares fished by year
    temp <- aggregate(oms ~logdata$year, FUN = length)
  # fill in missing years
    temp2 <- seq(min(temp[,1]),max(temp[,1]))
    oms.freq <- cbind(temp2,rep(0,length(temp2)))
    oms.freq[match(temp[,1],oms.freq[,1]),2] <- temp[,2]
    oms.freq <- as.data.frame(oms.freq)
    colnames(oms.freq) <- c("year", "OMS")
if(!returnData){
  return(ggplot(oms.freq,aes(x=year,y=OMS))+
    geom_point(size=2)+geom_line()+
    theme_bw()+
    labs(x="",y="One minute squares fished")+
    scale_x_continuous(breaks=seq(min(oms.freq$year),max(oms.freq$year),2))+
    theme(axis.text.x=element_text(angle=45,hjust=1),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank()))}

if(returnData){return(oms.freq)}

} #end function
