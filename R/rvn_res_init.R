#' Create initial conditions file for Reservoirs
#'
#' rvn_res_init is used to write an initial conditions (rvc) format file for Raven,
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
#' @param initial_stage an optional double for percentage of maximum stage to
#' use as initial condition; default 0.0
#' @param output file rvc lines are written to (default: initial_res_conditions.rvc)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{rvn_res_read}} for reading in the ReservoirStages.csv file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven reservoir stage relationship initial conditions
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' # File location
#' ff <- system.file("extdata","ReservoirStages.csv", package="RavenR")
#'
#' #-- Output file name
#' temprvc <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".rvc")
#'
#' # Run function
#' rvn_res_init(ff, initial_stage=0.4, output=temprvc)
#' }
#'
#' @export rvn_res_init
rvn_res_init <- function(ff, initial_stage=0.0, output="initial_res_conditions.rvc") {

  if (initial_stage < 0 || initial_stage > 1) {
    stop("Initial stage must be between 0 and 1.")
  }

  #res <- read.csv(ff,header=T)
  res <- rvn_res_read(ff)
  resdf <- res$res

  # initiate files
  fc2 <- file(tempfile, open='w+a')
  writeLines(c('# Initial conditions for reservoirs in a Raven model',
               '# Copy + Paste these lines into the rvc file',
               '#'), fc2)

  #-- Regex Magic - get subbasin columns, ignore obs
  sb_cols <- grep('sub.[0-9]$',colnames(resdf), ignore.case = T, )
  sb_id <- as.numeric(gsub('sub','',colnames(resdf)[sb_cols], ignore.case = T))

  for (i in 1:length(sb_cols)) {
    stage <- coredata(resdf[,sb_cols[i]])
    value <- min(stage) + initial_stage * (max(stage) - min(stage))
    writeLines(sprintf(':InitialReservoirStage  %i  %.2f',sb_id[i], value), fc2)
  }
  close(fc2)

  return(TRUE)
}
