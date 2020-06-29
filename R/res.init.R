#' Create initial conditions file for Reservoirs
#'
#' res.init is used to write an initial conditions (rvc) format file for Raven,
#' with the calculated reservoir stages written in.
#'
#' This function is used to write an initial conditions format file for Raven
#' with the relevant initial reservoir stages. This file can be used directly
#' as the model rvc file, or one may copy and paste the information into a
#' separate rvc file for use (i.e. if there is other information to be included
#' in the model rvc file).
#'
#' The supplied file in ff should be a csv file with the format headers of:
#' SUBID,STAGE,FLOW,VOL,AREA. The header text is not important as the function
#' assumes the order only, however some header must exist. The SUBID will be
#' the integer of the subbasin where the reservoir is located, and will repeat
#' the value for each point in the stage relationship provided. This is the
#' only required variable. This format is the same as is requried for the
#' res.fit function (thus the extra columns that are not used by this
#' function).
#'
#' Each point in the stage relationship table is used, so if there exist
#' multiple dry inflow points, the minimum stage of these points will be used
#' as the 'minimum' stage.
#'
#' Also note that the initial stage must be between 0 and 1, so that the
#' initial stage is not less than the dry stage or greater than the maximum.
#'
#' @param ff full file path to the reservoir information file
#' @param initial.stage an optional double for percentage of maximum stage to
#' use as initial condition; default 0.0
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{res.read}} for reading in the ReservoirStages.csv file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven reservoir stage relationship initial conditions
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' # set working directory to file location
#' dir = "C:/temp/test"
#' ff <- paste0(dir,"/","reservoir data.csv")
#'
#' res.init(ff,initial.stage=0.4)
#' }
#'
#' @export res.init
res.init <- function(ff=NA,initial.stage=0.0) {

  if (missing(ff)) {
    stop("ff as a file path to reservoir data is required for this function.")
  }

  if (initial.stage < 0 || initial.stage > 1) {
    stop("Initial stage must be between 0 and 1.")
  }

  res <- read.csv(ff,header=T)

  # initiate files
  fc2 <- file("initial_res_conditions.rvc.temp",open='w+a')
  writeLines(c('# Initial conditions for reservoirs in a Raven model','# Copy + Paste these lines into the rvc file','#'),fc2)

  subbasins <- unique(res[,1])

  for (i in 1:length(subbasins)) {
    dd <- res[res[,1] == subbasins[i],]

    stage <- dd[,2]
    writeLines(sprintf(':InitialReservoirStage  %i  %.2f',subbasins[i],min(stage)+initial.stage*(max(stage)-min(stage))),fc2)
  }
  close(fc2)

  return(TRUE)
}
