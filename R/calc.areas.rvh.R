#' Calculate areas from Raven .rvh file
#'
#' calc.areas.rvh is used to calculate the total area and drainage area for
#' each subbasin, from a Raven-formatted .rvh file
#'
#' This function reads in the .rvh file in Raven format, and firstly calculates
#' the total area of each subbasin by linking the IDs to the HRU section.
#'
#' The second piece of functionality is to use the network connections in the
#' Subbasins section to calculate the drainage area of each subbasin. The basic
#' step by step implementation of the code is as follows: 1) for all headwater
#' basins, assign drainage.area = basin.area 2) iterate through all subbasins;
#' if the drainage area is not calculated, calculate it if all basins that feed
#' water to that basin have their drainage areas calculated, otherwise skip 3)
#' repeat step 2 until all drainage areas are no longer NA 4) break if the
#' iter.thresh is exceeded for the number of iterations of Step 2
#'
#' This is a simple drainage area calculator that works for any .rvh
#' configuration, as long the connections are feasible.
#'
#' Note that the reading of the .rvh file currently assumes that there are no
#' comments on lines between :SubBasin and :EndSubBasins, nor in the HRUs
#' block.
#'
#' @param ff full filepath to .rvh file
#' @param calc.da boolean to calculate drainage areas
#' @param write.file boolean to write data to file
#' @param iter.thresh value to overwrite default iteration threshold in drainage
#' area calculation
#' @return \item{areas.rvh}{matrix with calculated areas for each subbasin}
#' @seealso \code{\link{forcings.read}} for analysis tools related to the
#' forcing functions
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven rvh area drainage
#' @examples
#'
#' # locate in RavenR rvh sample file
#' ff <- system.file("extdata","Nith.rvh", package="RavenR")
#'
#' # basic, with defaults (calculates drainage area also)
#' myareas <- calc.areas.rvh(ff)
#'
#' # avoid calculating drainage areas
#' myareas <- calc.areas.rvh(ff,calc.da=F)
#'
#' # calculate drainage areas, adjust iteration threshold
#' myareas <- calc.areas.rvh(ff,iter.thresh=1000)
#'
#' # write results to file
#' myareas <- calc.areas.rvh(ff,write.file=T)
#'
#' @export calc.areas.rvh
calc.areas.rvh <- function(ff,calc.da=T,write.file=F,iter.thresh=NA) {

  if (missing(ff)) { stop("Requires rvh file as ff")}

  if (!(file.exists(ff))) { stop("Requires valid filepath to rvh file; check ff")}

  txt <- readLines(ff)

  # read in subbasin data
  sub.ind <- grep(':SubBasins',txt)
  sub.ind.end <- grep(':EndSubBasins',txt)
  sub.rvh <- matrix(NA,nrow=(sub.ind.end-sub.ind-3),ncol=4)
  colnames(sub.rvh) <- c('sub','downstr_sub','area_km2','drain_area_km2')
  sub.rvh.count = 1
  for (i in ((sub.ind+3):(sub.ind.end-1))) {

    line <- unlist(strsplit(txt[i],c('\t',' ')))
    line <- line[line != ""]

    sub.rvh[sub.rvh.count,] = as.numeric(c(line[1],line[3],NA,NA))
    sub.rvh.count <- sub.rvh.count + 1
  }

  # read in HRU data
  hru.ind <- grep(':HRUs',txt)
  hru.ind.end <- grep(':EndHRUs',txt)
  hru.rvh <- matrix(NA,nrow=(hru.ind.end-hru.ind-3),ncol=3)
  colnames(hru.rvh) <- c('hru','subID','area_km2')
  hru.rvh.count = 1
  for (i in ((hru.ind+3):(hru.ind.end-1))) {

    line <- unlist(strsplit(txt[i],c('\t',' ')))
    line <- line[line != ""]

    hru.rvh[hru.rvh.count,] = as.numeric(c(line[1],line[6],line[2]))
    hru.rvh.count <- hru.rvh.count + 1
  }

  # combine subbasin data and total areas
  for (i in 1:nrow(sub.rvh)) {
    temp <- which(hru.rvh[,2] == sub.rvh[i,1])
    tot.area <- sum(hru.rvh[temp,3])
    sub.rvh[i,3] <- tot.area
  }

  ### calculate all drainage areas

  if (calc.da) {

    # assign drainage_area = area in all headwater basins
    for (i in 1:nrow(sub.rvh)) {

      drains.to <- which(sub.rvh[,2] == sub.rvh[i,1])

      if (length(drains.to) == 0) {
        sub.rvh[i,4] <- sub.rvh[i,3]
      }
    }

    # compute all remaining drainage areas iteratively

    iter.count <- 0
    if (is.na(iter.thresh)) { iter.thresh <- nrow(sub.rvh)*10 }
     # max number of iterations as 10*number_subs by default, failsafe if sub not found

    # continue script as long as there are uncalculated drainage areas
    while(any(is.na(sub.rvh[,4]))) {

      for (i in 1:nrow(sub.rvh)) {

        # if the drainage_area is NA, try to calculate
        if (is.na(sub.rvh[i,4])) {

          # find which basins drain into this one
          drains.to <- which(sub.rvh[,2] == sub.rvh[i,1])

          # check if their drainage areas have not been calculated
          # if not, then calculate this drainage area
          if (!(any(is.na(sub.rvh[drains.to,4])))) {
            sub.rvh[i,4] <- sum(sub.rvh[drains.to,4]) + sub.rvh[i,3]
          }
        }
      }

      iter.count <- iter.count + 1

      if (iter.count > iter.thresh) {
        warning(sprintf("Iteration limit exceeded %i iterations; check rvh file for extra comments or broken links.",iter.thresh))
        break
      }
    }
  }

  if (write.file) {
    # write sub.rvh to file
    write.csv(sub.rvh, file=sprintf('%s_calculated_areas.csv',strsplit(ff,'.rvh')[[1]]),row.names=F,quote=F)
  }

  return("areas.rvh" = sub.rvh)
}
