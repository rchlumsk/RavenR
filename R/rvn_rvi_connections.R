#' @title Generate Hydrological process connections list
#'
#' @description
#' This routine reads in a hydrologic process list from \code{\link{rvn_rvi_read}} and generates
#' the list of hydrologic process connections.
#'
#' @details
#' Relies on a valid and up-to-date RavenProcessConnections.dat file.
#' This file is provided with the RavenR package, but
#' may be overridden by a more recent file if provided manually.
#'
#' @param rvi data object generated from the \code{\link{rvn_rvi_read}} routine
#' @param ProcConDataFile (optional) path to RavenProcesConnections.dat file
#'
#' @return
#' Returns a list with two items:
#' \item{connections}{a a dataframe of all of the process connections Includes the following data columns:
#' process type, algorithm, 'from' compartment, 'to' compartment, and conditional}
#' \item{AliasTable}{a table of aliases (unchanged from the supplied \code{rvi$AliasTable})}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @seealso \code{\link{rvn_rvi_read}} to read a .rvi file and generate an rvi object, and
#' \code{\link{rvn_rvi_process_ggplot}} or \code{\link{rvn_rvi_process_diagrammer}} to plot the process network produced in this function.
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#' rvi <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR"))
#'
#' rvi_conn <- rvn_rvi_connections(rvi)
#' head(rvi_conn$connections)
#' head(rvi_conn$AliasTable)
#'
#' @export rvn_rvi_connections
rvn_rvi_connections<-function(rvi,ProcConDataFile=system.file("extdata","RavenProcessConnections.dat", package="RavenR"))
{
   HPTable <- rvi$HydProcTable
   AliasTable <- rvi$AliasTable

   # update condition and conditional here
   # update reference to RavenProcessConnections.dat with new files

   if (nrow(HPTable)==0)
   {
      print("WARNING (rvn_rvi_connections): no rows in hydrologicprocess table")
      return (NA)
   }
   if (paste(names(HPTable), collapse=" ") != "ProcessType Algorithm From To Conditional Note")
   {
      #  & ncol(HPTable)!=6
      stop("(rvn_rvi_connections): improper hydrological process table data frame format")
   }
   stopifnot(file.exists(ProcConDataFile))
   delim=""
   cnames<-c("Algorithm","ProcessType","From","To")
   # print(paste0("about to read ",ProcConDataFile))
   ProcConnTable<-read.table(ProcConDataFile,
                             sep=delim,
                             col.names=cnames,
                             header=TRUE,
                             blank.lines.skip=TRUE,
                             strip.white=TRUE,
                             stringsAsFactors=FALSE,
                             flush=TRUE,
                             comment.char = "#")

   # to generate process list, select all rows from ProcConnTable where algorithm
   # is present in HPTable$Algorithm

   missingAlgs<-connections <- data.frame(row.names=row.names(HPTable)) # blank frame

   for (i in 1:nrow(HPTable))
   {
      tmp<-ProcConnTable[ProcConnTable$Algorithm %in% HPTable$Algorithm[i],] # gets set of all connections
      if (nrow(tmp)==0)
      {
         missingAlgs<-rbind(missingAlgs,HPTable[i,])
      }else{
         #handle user-specified connections
         for (j in 1:nrow(tmp))
         {
            if (tmp$From[j]=="SOIL/AQUIFER" | tmp$From[j]=="SOIL" | tmp$From[j]=="USER_SPECIFIED")
            {
               tmp$From[j]=HPTable$From[i]
            }
            if (tmp$To[j]=="SOIL/AQUIFER" | tmp$To[j]=="SOIL" | tmp$To[j]=="USER_SPECIFIED")
            {
               tmp$To[j]=HPTable$To[i]
            }
            tmp$Conditional[j]=HPTable$Conditional[i] # copy conditional over from .rvi
            tmp$Note[j] <- HPTable$Note[i] # copy over information from rvi
         }
         connections<-rbind(connections,tmp) # append
      }
   }
   if(ncol(missingAlgs) > 0)
   {
      print(paste0("WARNING: algorithm(s) ",missingAlgs$Algorithm," not found in master list."))
   }
   return(list(connections=connections, AliasTable=AliasTable))
}
