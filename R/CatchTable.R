# Catch-Effort Table
#' @title Extract the Catch and effort data as a flattend data.frame
#' @description CatchTable
#' @param logdata is the processed log data from the functions GetLogData() and ProcessLogData().
#' @param SelBank a numeric delimiter of the assessment region (1 = Banquereau and 2 = Grand Bank).
#' @param Area (default=NA) is a optional indicator to flag the size of the assessment area (bank). If NA Banquereau and Grand Bank are assumed to be 10908.1 & 49473.0 km2 respectively
#' @param tableNames defines whether the table names should be returned as expression (e.g. superscripts)
#' @rdname CatchTable
#' @author Brad Hubley and Ryan Stanley
#' @export

CatchTable <- function(logdata, SelBank, Area=NA,tableNames=FALSE){
  ##############################################################################
  ## catch and effort table, still using same data
  ##############################################################################
  names(logdata) <- toupper(names(lodata))

  logdata <- logdata[logdata$BANK == SelBank,]

  temp <- aggregate(cbind(ROUND_CATCH / 1000, AREA  /1000000) ~YEAR,
                    data = logdata, sum)
  ## fill in missing years
  yy <- seq(min(temp[, 1]), max(temp[, 1]))
  table.1 <- cbind(yy, matrix(rep(0, 2 * length(yy)), length(yy), 2))
  table.1[match(temp[, 1], table.1[, 1]), 2] <- temp[, 2]
  table.1[match(temp[, 1], table.1[, 1]), 3] <- temp[, 3]

  if(SelBank == 1 && is.na(Area)){
    Area <- 10908.1 ## area within 100m contour of Banquereau Bank
  }else if(SelBank == 2 && is.na(Area)){
    Area <- 49473.0 ## original survey area on Grand Bank
  }

  table.1  <- cbind(table.1, 100.0 * table.1[,3] / Area)
  table.1 <- cbind(table.1[,1], round(table.1[,2], 0),
                   round(table.1[,3], 1),
                   rep(0,length(yy)),
                   round(table.1[,4],2))
  idx <- which(table.1[,3] != 0)
  table.1[idx,4] <- round(table.1[idx,2] / table.1[idx,3], 0)

  if(tableNames) # if you want the formal table names for table output
    {
  colnames(table.1) <- c("Year", "Logged Catch (t)",
                         paste("Area Dredged (km", "\u00B2", ")", sep = ""),
                         "CPUE", "% Area")
    }

  if(!tableNames) # return for function input
  {
    colnames(table.1) <- c("Year","Logged_Catch","Area_Dredged","CPUE","Area")
  }

  return(as.data.frame(table.1))
}
