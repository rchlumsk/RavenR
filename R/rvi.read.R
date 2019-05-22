#' Read Raven .rvi (watershed discretization) file
#'
#' This routine reads in a valid Raven main input (.rvi) file and returns the
#' information about hydrological processes as a data table.
#'
#' @param filename the name of the .rvi file (with .rvi extension included ), either relative
#' to the working directory or absolute.
#'
#' @return
#' Returns a list including a lone item:
#' \item{HydProcTable}{a data table of hydrologic processes. Includes the following data columns:
#' process type, algorithm, 'from' compartment, 'to' compartment, and condition}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @note does not like tabs in the .rvi file - it should be untabified first.
#' The .rvi file can have arbitrary contents outside of the :HydrologicProcesses-
#' :EndHydrologicProcesses block and :SubBasins-:EndSubBasins command blocks.
#'
#' @details does not like comma-delimited tables with a trailing comma
#'
#' @seealso  rvi.connections
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#'  \dontrun{
#'   # sample workflow of rvh.read
#'
#'   rvi <- rvi.read("example.rvi")
#'
#'   # get number of Hydrologic processes
#'   numProcss <- nrow(rvi$HydProcTable)
#'}
#' @keywords Raven  rvi  Hydrologic Processes read
#' @export rvi.read
rvi.read<-function(filename)
{
  stopifnot(file.exists(filename))

  # read subbasins table--------------------------------
  lineno<-grep(":HydrologicProcesses", readLines(filename), value = FALSE)
  lineend<-grep(":EndHydrologicProcesses", readLines(filename), value = FALSE)

  if ((length(lineno)==0) || (length(lineend)==0)){
    print('warning: filename not a valid .rvi file (no :HydrologicProcesses block)')
  }
  delim=""
  if (length(grep(",", readLines(filename)[(lineno+3):(lineend-1)], value = FALSE))>0){
    delim="," # comma delimited
  }
  cnames<-c("ProcessType","Algorithm","From","To")
  #print(paste0("read rvi: |",delim,"| ",lineno," ",lineend," ",lineend-lineno-3 ))
  HPTable<-read.table(filename,
                          skip=lineno,
                          nrows=lineend-lineno-1,
                          sep=delim,
                          col.names=cnames,
                          header=FALSE,
                          blank.lines.skip=TRUE,
                          strip.white=TRUE,
                          stringsAsFactors=FALSE,
                          fill=TRUE, #fills empty data columns
                          flush=TRUE, #ignores extra data columns
                          comment.char = "#")
  HPTable$ProcessType<-trimws(HPTable$ProcessType)
  HPTable$Algorithm  <-trimws(HPTable$Algorithm)
  HPTable$From       <-trimws(HPTable$From)
  HPTable$To         <-trimws(HPTable$To)
  #untabify
  #HPTable <- as.data.frame(sapply(HPTable, function(x) gsub("\t", "", x)))

  # correct rviTable for reading RAVEN_DEFAULT algs
  for (i in 1:nrow(HPTable)){
    if (HPTable$Algorithm[i]=="RAVEN_DEFAULT") { # not unique
      if (HPTable$ProcessType[i]==":-->Overflow"       ){HPTable$Algorithm[i]="OVERFLOW_RAVEN"}
      else if (HPTable$ProcessType[i]==":Split"        ){HPTable$Algorithm[i]="SPLIT_RAVEN"}
      else if (HPTable$ProcessType[i]==":Precipitation"){HPTable$Algorithm[i]="PRECIP_RAVEN"}
      else if (HPTable$ProcessType[i]==":Flush"        ){HPTable$Algorithm[i]="FLUSH_RAVEN"}
    }
  }

  #add conditionals column
  HPTable$Conditional<-NA
  for (i in 1:nrow(HPTable))
  {
    is_cond=(HPTable$ProcessType[i]==":-->Conditional")
    #print(paste0("reading row ",i,": ",HPTable$Algorithm[i]))
    if (is_cond)
    {
      condname<-paste0(HPTable$Algorithm[i]," ",HPTable$From[i]," ",HPTable$To[i])
     # print(paste0("Conditional! ",condname))
      HPTable$Conditional[i-1]<-condname
    }
  }
  #delete conditionals rows
  HPTable<-HPTable[(HPTable$ProcessType!=":-->Conditional"),]

  return (list(HydProcTable=HPTable))
}
