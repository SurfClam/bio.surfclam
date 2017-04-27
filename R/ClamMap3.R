#' ClamMap
#' @param area = 'custom' where xlim & ylim are specified or select from area list below
#' @param logdata = is the processed log data from the functions GetLogData() and ProcessLogData().
#' @param SelBank = an idicator of the bank which corresponds to the 'bank' variable in logdata. By default 1 is Banquereau and 2 is Grand Bank.
#' @param mapRes = coastline detail ('LR' = low resolution, 'MR' = medium resolution, 'HR' = high resolution, 'UR' = ultra resolution)
#' @param title = plot title
#' @param boundaries = for ploting specific management boundaries
#' @param isobath = plots bathymetry lines for specified depths from topex data
#' @param bathcol = isobath line color, default is transparent blue
#' @param topolines = plots topographic lines for specified elevations from topex data
#' @param bathcol = topolines line color, default is transparent brown
#' @param points.lst = points to overlay on map in PBSmapping format - list with 2 elements: 1st element is eventSet (EID, POS, X, Y), 2nd element is eventData (EID, pch, col, etc.)
#' @param lines.lst = lines to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, lty, col, etc.)
#' @param poly.lst = polygons to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, border, col, etc.)
#' @param contours = plots overlaping polygons as contours (same format as poly.lst)
#' @param image.lst = image to overlay on map - list with 3 elements (x, y, z), 'bathymetry' produces image from bathymetry data
#' @param color.fun = color function for image
#' @param zlim = zlim for image
#' @param grid = size of grid in degrees, default is no grid
#' @param stippling = adds stippling to land (purely for visual effect)
#' @param lol = adds water colored border to coastline (purely for visual effect)
#' @import PBSmapping
#' @import fields
#' @import bio.base
#' @author Brad Hubley and Ryan Stanley
#' @examples
#' ClamMap2(area='Ban')
#' @export

ClamMap3<-function(logdata,SelBank,area='custom',ylim=c(40,52),xlim=c(-74,-47),mapRes='HR',land.col='wheat',title='',nafo=NULL,boundaries='clam',bathy.source='topex',isobaths=seq(100,1000,100),bathcol=rgb(0,0,1,0.1),topolines=NULL,topocol=rgb(0.8,0.5,0,0.2),points.lst=NULL,pt.cex=1,lines.lst=NULL,poly.lst=NULL,contours=NULL,image.lst=NULL,color.fun=tim.colors,zlim=NA,grid=NULL,stippling=F,lol=F,labels=NULL,labcex=1.5,LT=T,plot.rivers=T,addSummerStrata=F,addsubareas=F,...){

  # Custom area
  if(area=='custom')	{ ylim=ylim; 			xlim=xlim			}

  ## Area List
  if(area=='all')		{ ylim=c(42.5,48); 		xlim=c(-64,-47)	}
  if(area=='SS')		{ ylim=c(41,47); 		xlim=c(-68,-57)		}
  if(area=='ESS')		{ ylim=c(43,45.4); 		xlim=c(-62.5,-57.4)	}
  if(area=='WSS')		{ ylim=c(41,44); 		xlim=c(-67.3,-64)	}
  if(area=='BBn')		{ ylim=c(42.4,43); 		xlim=c(-66.6,-65.6)	}
  if(area=='BBs')		{ ylim=c(42.25,42.75); 	xlim=c(-66,-65.25)	}
  if(area=='BB')		{ xlim=c(-66.5,-65.25); ylim=c(42.25,43)	}
  if(area=='GB')		{ ylim=c(41.1,42.3); 	xlim=c(-67.3,-65.6)	}
  if(area=='GBb')		{ ylim=c(41.6,42.3); 	xlim=c(-66.7,-65.6)	}
  if(area=='Ger')		{ ylim=c(42.8,43.8); 	xlim=c(-67,-65)		}
  if(area=='Sab')		{ ylim=c(42.8,44.5); 	xlim=c(-62.5,-58.8)	}
  if(area=='West')	{ ylim=c(43,44.1); 		xlim=c(-62.2,-60.4)	}
  if(area=='Mid')		{ ylim=c(44.2,44.9);	xlim=c(-61.3,-60.1) }
  if(area=='Ban')		{ ylim=c(43.7,45.2); 	xlim=c(-60.5,-57)	}
  if(area=='SPB')		{ ylim=c(44.5,47.5);	xlim=c(-58,-55)		}
  if(area=='Grand')	{ ylim=c(43,46.5); 		xlim=c(-51.5,-48.5)		}
  if(area=='Grand2')	{ ylim=c(42.5,48); 		xlim=c(-55,-47)		}

  coast<-read.csv(file.path(bio.base::project.datadirectory("bio.surfclam"), "data","maps","gshhs",paste0("shoreline",mapRes,".csv")))
  rivers<-read.csv(file.path(bio.base::project.datadirectory("bio.surfclam"), "data","maps","gshhs",paste0("rivers",mapRes,".csv")))
  attr(coast,"projection")<-"LL"

  #Get the data for plotting points
  names(logdata) <- tolower(names(logdata))

  ## get last year in log data
  max.data.year <- max(as.numeric(format(logdata$record_date,"%Y")),na.rm=T)
  ## Subset data with non NA Latitude and Longitude
  MapData = subset(logdata,logdata$bank == SelBank
                   & (is.finite(logdata$lat_dd) & is.finite(logdata$lon_dd))
                   ## time difference in weeks from today, use 3 years as data
                   ## base not up to date
                   & as.numeric(format(logdata$record_date,"%Y")) >
                     max.data.year - 3)
  idx <- sort(as.numeric(format(MapData$record_date,"%Y")), index.return = TRUE,
              decreasing = TRUE)
  MapData <- MapData[idx$ix,]
  idx.year.1 <- which(format(MapData$record_date,"%Y") ==
                        as.character(max.data.year - 2))
  idx.year.2 <- which(format(MapData$record_date,"%Y") ==
                        as.character(max.data.year - 1))
  idx.year.3 <- which(format(MapData$record_date,"%Y") ==
                        as.character(max.data.year))
  ln <- dim(MapData)[1] ## save number of records in file
  ## make into data frame for plotting
  fish.points <- data.frame(PID = seq(1, ln),POS = seq(1, ln),
                            X = MapData$lon_dd, Y = MapData$lat_dd)
  fish.points <- as.PolyData(fish.points) ## Chage to PolyData for plotting
  attr(fish.points, "projection") <- "LL"
  ## Make polyProps file = Properties file for plotting
  PID <- seq(1, ln)
  pch <- rep(20, ln)
  col <- rep("red", ln)
  col[idx.year.2] <- rep("green", length(idx.year.2))
  col[idx.year.1] <- rep("yellow", length(idx.year.1))
  cex <- rep(0.5, ln)
  ## Now draw map, add NAFO lines and labels (nafo = 'all'),
  ## add Bank labels (banks = T)
  Points_Par <- data.frame(PID, pch = I(pch), col = I(col), cex = I(cex))
  attr(Points_Par, "projection") <- "LL"

  if(is.null(points.lst)){points.lst = list(fish.points, polyProps = Points_Par)}

  #par(...)
  plotMap(coast,xlim=xlim,ylim=ylim,border=NA,...)
  #addLines(rivers)

  if(lol)addPolys(coast,border=bathcol,lwd=6)

  # Image
  if(!is.null(image.lst)){
    if(missing(zlim))zlim<-range(image.lst$z,na.rm=T)
    image(image.lst,add=T,col=color.fun(100),zlim=zlim)
  }

  # plot polygons
  if(!is.null(contours)){
    contours[[2]]<-subset(contours[[2]],PID%in%contours[[1]]$PID)
    junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))
    for(i in unique(contours[[2]]$PID)){
      addPolys(joinPolys(subset(contours[[1]],PID==i),junk,operation="DIFF"),polyProps=contours[[2]])
    }
  }
  if(!is.null(poly.lst)){
    addPolys(poly.lst[[1]],polyProps=poly.lst[[2]])
  }


  # Bathymetry
  sn<-ifelse(sum(isobaths/10)==sum(round(isobaths/10)),"",1)
  #browser()
  if(!is.null(isobaths)){
    bath.lst<-list()
    for(i in unique(ceiling(isobaths/1000))){
      load(file.path( datadir, "data","maps", bathy.source, paste0("bathy",sn,"Poly",i,".rdata")))
      bath.lst[[i]]<-bathy.poly
    }
    bathy.poly<-do.call(rbind,bath.lst)
    #browser()
    bathy.poly<-subset(bathy.poly,Z%in%isobaths)
    attr(bathy.poly,"projection") <- "LL"
    addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID),col=bathcol,stringsAsFactors = F))
    #browser()
  }

  #NAFO
  	if(!is.null(nafo)){

          nafo.xy<-read.csv(file.path( datadir, "data","maps","nafo.csv"))
          if(nafo[1]=='all')nafo<-unique(nafo.xy$label)
          nafo.sel<-subset(nafo.xy,label%in%nafo)
          nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
          nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"
          attr(nafo.xy,"projection") <- "LL"

  		addPolys(nafo.xy,border='grey',col=NULL)
  		addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)
  	}


  #groundfish survey summer strata
  if(addSummerStrata) {
    loadfunctions('polygons')
    a = find.ecomod.gis('summer_strata_labels',return.one.match=F)
    a = read.csv(a,header=T)
    names(a)[4] <- 'label'
    b = find.ecomod.gis('strat.gf',return.one.match=F)
    b = read.table(b)
    names(b) <- c('X','Y','PID')
    b = within(b,{POS <- ave(PID,list(PID),FUN=seq_along)})
    addPolys(b,lty=1,border='black',col=NULL)
    # addLabels(a,cex=0.6)
  }
  # Boundries
  EEZ<-read.csv(file.path( datadir, "data","maps","EEZ.csv"))
  addLines(EEZ,lty=4,lwd=2)

  # plots land
  if(LT){
    addPolys(coast,col=land.col,...)
    if(plot.rivers)addLines(rivers,...)
  }

  if(stippling)addStipples (coast, pch='.')

  # Topography

  if(!is.null(topolines)){
    topo.lst<-list()
    for(i in unique(ceiling(topolines/1000))){
      load(file.path( datadir, "data", "maps","topex",paste0("topoPoly",i,".rdata")))
      topo.lst[[i]]<-topo.poly
    }
    topo.poly<-do.call(rbind,topo.lst)
    topo.poly<-subset(topo.poly,Z%in%topolines)
    attr(topo.poly,"projection") <- "LL"
    addLines(topo.poly,polyProps=data.frame(PID=unique(topo.poly$PID),col=topocol))
  }

  # plot points
  #if(!is.null(points.lst)){
  addPoints(points.lst[[1]],polyProps=points.lst[[2]],cex=pt.cex)
  #}

  # plot lines
  if(!is.null(lines.lst)){
    addLines(lines.lst[[1]],polyProps=lines.lst[[2]])
  }

  # add grid lines
  if(!is.null(grid)){
    x<-seq(floor(xlim[1]),ceiling(xlim[2]),grid)
    y<-seq(floor(ylim[1]),ceiling(ylim[2]),grid)
    gridlines<-makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection="LL",zone=NULL)
    addLines(gridlines,col='grey80',lwd=1)
  }

  if(is.list(labels)) addLabels(labels[[1]],polyProps=labels[[2]])

  box(lwd=2)

  title(main=title)

}
