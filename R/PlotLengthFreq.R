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
#' @importFrom data.table data.table
#' @export


PlotLengthFreq=function(lfdata,SelBank,stat="cdf") {

  #Manipulate data for plotting
  expandlf <- data.table::data.table(lfdata)
  expandlf <- data.frame(expandlf[rep(seq(1,nrow(expandlf)),expandlf$NUM)])

#Plotting parameters and data filtering
  if(SelBank==2){sizeThresh=105
  expandlf <- dplyr::filter(expandlf,AREA==3,SHELL_LEN<=200)}

  if(SelBank==1){sizeThresh=120
  expandlf <- dplyr::filter(expandlf,AREA==4,SHELL_LEN<=200)}

#Plot limits
  MaxLimit <- floor(lfdata%>%group_by(YEAR)%>%
                    mutate(prec=NUM/sum(NUM)*100)%>%
                    ungroup()%>%
                    summarise(Max=max(prec,na.rm=T))%>%as.integer())

  #Numbers of clams in each year
  Numbers <- expandlf%>%group_by(YEAR)%>%
              summarise(num=length(NUM))%>%ungroup()%>%data.frame()

#Plots
  if(stat=="lf"){

    #Text locations
    Numbers$x=20;Numbers$y=MaxLimit-1.5

    print(
      ggplot()+
    geom_density(data=expandlf,aes(x=SHELL_LEN,y=..density..*100),lty=2)+
    geom_text(data=Numbers,aes(x=x,y=y,label=paste0("n=",num)))+
    facet_grid(YEAR~.)+
    geom_vline(xintercept=sizeThresh)+
    theme_bw()+
    labs(x="Shell length (mm)",y="Frequency (%)" )+
    scale_y_continuous(breaks=c(0,(MaxLimit-1)/2,MaxLimit-1),
                       labels=c("0","",as.character(MaxLimit-1)))+
      theme(strip.background = element_rect(colour = "black", fill = "white")))
    }

  if(stat=="cdf"){

   #Text locations
    Numbers$x=20;Numbers$y=0.8

  print(ggplot()+
    stat_ecdf(data=expandlf,aes(x=SHELL_LEN))+
    geom_text(data=Numbers,aes(x=x,y=y,label=paste0("n=",num)))+
    facet_wrap(~YEAR,ncol=3,scales="free_y")+
    geom_vline(xintercept=sizeThresh)+
    theme_bw()+
    labs(x="Shell length (mm)",y="Cumlative frequency (%)" )+
      theme(strip.background = element_rect(colour = "black", fill = "white")))
    }

} #end function
