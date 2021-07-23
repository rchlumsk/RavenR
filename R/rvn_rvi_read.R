#' @title Read Raven .rvi (watershed discretization) file
#'
#' @description
#' This routine reads in a valid Raven main input (.rvi) file and returns the
#' information about hydrological processes as a data table.
#'
#' @param filename the name of the .rvi file (with .rvi extension included ), either relative
#' to the working directory or absolute.
#'
#' @return
#' Returns a list with two items:
#' \item{HydProcTable}{a data table of hydrologic processes. Includes the following data columns:
#' process type, algorithm, 'from' compartment, 'to' compartment, conditional (logical), and condition (character)}
#' \item{AliasTable}{a table of aliases read from the rvi file}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @details
#' This function does not like tabs in the .rvi file - it should be untabified first. Comma-delimited tables with a trailing comma are also problematic.
#' The .rvi file can have arbitrary contents outside of the :HydrologicProcesses-
#' :EndHydrologicProcesses block and :SubBasins-:EndSubBasins command blocks.
#'
#' @examples
#' # sample workflow of rvn_rvi_read
#' rvi <- system.file("extdata","Nith.rvi", package="RavenR") %>%
#' rvn_rvi_read(.)
#'
#' # get number of Hydrologic processes
#' nrow(rvi$HydProcTable)
#'
#' @export rvn_rvi_read
#' @importFrom utils read.table
rvn_rvi_read<-function(filename)
{
  # checks and setup ----
  stopifnot(file.exists(filename))

  # read subbasins table--------------------------------
  lineno<-grep(":HydrologicProcesses", readLines(filename), value = FALSE)
  lineend<-grep(":EndHydrologicProcesses", readLines(filename), value = FALSE)

  if ((length(lineno)==0) || (length(lineend)==0)){
    warning('filename not a valid .rvi file (no :HydrologicProcesses block)')
  }
  delim="\\s+"
  if (length(grep(",", readLines(filename)[(lineno+3):(lineend-1)], value = FALSE))>0){
    delim="," # comma delimited
  }

  cnames<-c("ProcessType","Algorithm","From","To","Conditional","Note")
  HPTable <- data.frame(matrix(NA, nrow=(lineend-lineno-1), ncol=length(cnames)))
  colnames(HPTable) <- cnames
  HPtext <- readLines(filename)[(lineno+1):(lineend-1)]

  # parse text ----
  for (i in 1:length(HPtext)) {

    temp <- unlist(strsplit(trimws(HPtext[i]),split="\\s+"))

    if (temp[1] == ":LateralFlush") {

      if (length(temp) != 7) {
        warning(sprintf("Incorrect number of entries for :LateralFlush on line %i (expected 7)\n%s",i,HPtext[i]))
      } else {
        HPTable[i,1:4] <- temp[c(1,2,4,7)]
        HPTable[i,6] <- sprintf("LateralFlush from %s (%s) to %s (%s)",temp[3],temp[4], temp[6],temp[7])
      }
    } else if (temp[1] == ":Split") {
      HPTable[i,1:4] <- temp[1:4]
      HPTable[i,6] <- sprintf("ADD :Split %s %s %s",
                              temp[2], temp[3], temp[5])
    } else if (length(temp) == 4) {
      HPTable[i,1:4] <- temp
    }
  }

  # handle conditionals ----
  HPTable$Conditional <- ""
  HPTable$Note <- ""

  i <- 1
  while (i <= nrow(HPTable)) {

    if (i <= (nrow(HPTable)-1) & HPTable$ProcessType[i+1]==":-->Conditional") {

      cond_index <- i
      condname<-paste0(HPTable$Algorithm[i+1]," ",HPTable$From[i+1]," ",HPTable$To[i+1])
      i <- i+1

      if (HPTable$ProcessType[i+1]==":-->Conditional") {
        for (j in (i+1):nrow(HPTable)) {
          if (HPTable$ProcessType[j]==":-->Conditional") {
            condname <- sprintf("%s; %s",condname, paste0(HPTable$Algorithm[j]," ",HPTable$From[j]," ",HPTable$To[j]))
          } else {
            i <- j-1
            break
          }
        }
      }
      HPTable$Conditional[cond_index]<-condname
    }

    i <- i+1
  }

  # split separations ----
  while (length(grep("\\bADD :Split",x=HPTable$Note)) > 0) {

    i <- grep("\\bADD :Split",x=HPTable$Note)[1]

    temp <- data.frame(matrix(NA,nrow=1,ncol=length(cnames)))
    colnames(temp) <- cnames
    temp[1,1:4] <- unlist(strsplit(HPTable[i,"Note"], split=" "))[2:5]
    temp[1,5] <- HPTable[i,5] # carry over any conditional information
    # temp[1,6] <-
    HPTable[i,"Note"] <- ""
    HPTable <- rbind(HPTable[1:i,],
                     temp,
                     HPTable[(i+1):(nrow(HPTable)),])
  }

  # correct rviTable for reading RAVEN_DEFAULT algs ----
  for (i in 1:nrow(HPTable)){
    if (HPTable$Algorithm[i]=="RAVEN_DEFAULT") { # not unique
      if (HPTable$ProcessType[i]==":-->Overflow"       ){HPTable$Algorithm[i]="OVERFLOW_RAVEN"}
      else if (HPTable$ProcessType[i]==":Split"        ){HPTable$Algorithm[i]="SPLIT_RAVEN"}
      else if (HPTable$ProcessType[i]==":Precipitation"){HPTable$Algorithm[i]="PRECIP_RAVEN"}
      else if (HPTable$ProcessType[i]==":Flush"        ){HPTable$Algorithm[i]="FLUSH_RAVEN"}
      # else if (HPTable$ProcessType[i]==":LateralFlush"        ){HPTable$Algorithm[i]="LATERALFLUSH_RAVEN"} # leave as RAVEN_DEFAULT
    }
  }

  #delete conditionals rows
  HPTable<-HPTable[(HPTable$ProcessType!=":-->Conditional"),]

  # read aliases ----
  alias_df <- data.frame(matrix(NA,nrow=0,ncol=2))
  colnames(alias_df) <- c("Alias","Basename")
  for (ind in grep(":Alias", readLines(filename), value = FALSE)) {
    temp <- unlist(strsplit(readLines(filename)[ind],split=" "))
    temp <- temp[temp != ""]
    alias_df <- rbind(alias_df, data.frame(t(temp[2:3])))
  }

  return (list(HydProcTable=HPTable, AliasTable=alias_df))
}
