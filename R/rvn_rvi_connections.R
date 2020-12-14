#' @title Generate Hydrological process connections list
#'
#' @description
#' This routine reads in a hydrologic process list from rvn_rvi_read() and generates
#' the list of hydrologic process connections.
#'
#' @details
#' This function relies on a valid and up-to-date RavenProcessConnections.dat file.
#' This file is provided with the RavenR package, but
#' may be overrided by a more recent file if provided manually.
#'
#' @param rvi data object generated from the rvn_rvi_read() routine
#' @param ProcConDataFile (optional) path to RavenProcesConnections.dat file
#'
#' @return
#' Returns a dataframe of all of the process connections Includes the following data columns:
#' process type, algorithm, 'from' compartment, 'to' compartment, and conditional.
#'
#' @author James R. Craig, University of Waterloo
#'
#' @seealso \code{\link{rvn_rvi_read}} to read a .rvi file and generate an rvi object, and
#' \code{\link{rvn_rvi_process_plot}} to plot the process network produced in this function.
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#' rvi <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR"))
#'
#' # get number of Hydrologic processes
#' nrow(rvi$HydProcTable)
#'
#' conn <- rvn_rvi_connections(rvi)
#' head(conn)
#'
#' @export rvn_rvi_connections
rvn_rvi_connections<-function(rvi,
                              ProcConDataFile=system.file("extdata","RavenProcessConnections.dat", package="RavenR") ) {

  HPTable <- rvi$HydProcTable

  if (nrow(HPTable)==0){
    print("WARNING (rvn_rvi_connections): no rows in hydrologicprocess table")
    return (NA)
  }
  if (ncol(HPTable)!=5){
    print("WARNING (rvn_rvi_connections): improper hydrological process table data frame format")
    return (NA)
  }
  stopifnot(file.exists(ProcConDataFile))
  delim=""
  cnames<-c("Algorithm","ProcessType","From","To")
  #print(paste0("about to read ",ProcConDataFile))
  ProcConnTable<-read.table(ProcConDataFile,
                            sep=delim,
                            col.names=cnames,
                            header=FALSE,
                            blank.lines.skip=TRUE,
                            strip.white=TRUE,
                            stringsAsFactors=FALSE,
                            flush=TRUE,
                            comment.char = "#")


  # to generate process list, select all rows from ProcConnTable where algorithm
  # is present in HPTable$Algorithm

  connections <- data.frame(row.names=row.names(HPTable)) # blank frame
  for (i in 1:nrow(HPTable))
  {
    tmp<-ProcConnTable[ProcConnTable$Algorithm %in% HPTable$Algorithm[i],] # gets set of all connections
    if (nrow(tmp)==0){
      print(paste0("WARNING: algorithm ",HPTable$Algorithm[i]," not found in master list."))
    }
    else{
      #print(paste0(nrow(tmp)," connections for ", HPTable$Algorithm[i]," algorithm"))
      #handle user-specified connections
      for (j in 1:nrow(tmp)){
        if (tmp$From[j]=="USER_SPECIFIED"){
          tmp$From[j]=HPTable$From[i]
        }
        if (tmp$To[j]=="USER_SPECIFIED"){
          tmp$To[j]=HPTable$To[i]
        }
        tmp$Conditional[j]=HPTable$Conditional[i] # copy conditional over from .rvi
      }
      connections<-rbind(connections,tmp) # append
    }
  }
  return(connections)
}
