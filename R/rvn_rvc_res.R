#' @title Create initial conditions file for Reservoirs
#'
#' @description
#' rvn_rvc_res is used to write an initial conditions (rvc) format file for Raven,
#' with the calculated reservoir stages written in.
#'
#' @details
#' This function is used to write an initial conditions format file for Raven
#' with the relevant initial reservoir stages. This file can be used directly
#' as the model rvc file, or one may copy and paste the information into a
#' separate rvc file for use (i.e. if there is other information to be included
#' in the model rvc file).
#'
#' The supplied file in ff should be a csv file consistent with the format from the
#' Raven-generated ReservoirStages.csv file. External observations of reservoirs may be used
#' given that the csv file follows the same format.
#'
#' The initial_percent must be between 0 and 1, so that the
#' initial stage is not less than the dry stage or greater than the maximum.
#'
#' @param ff full file path to the reservoir information file
#' @param initial_percent an optional double for percentage of maximum stage to
#' use as initial condition; default 0.0
#' @param output file rvc lines are written to (default: initial_res_conditions.rvc)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{rvn_res_read}} for reading in the ReservoirStages.csv file
#'
#' @examples
#' ff <- system.file("extdata","ReservoirStages.csv", package="RavenR")
#'
#' # set initial conditions at 40% capacity, view file
#' tf <- file.path(tempdir(), "modelname.rvc")
#' rvn_rvc_res(ff, initial_percent=0.4, output=tf)
#' readLines(tf)
#'
#' @export rvn_rvc_res
rvn_rvc_res <- function(ff, initial_percent=0.0, output="initial_res_conditions.rvc")
{

  if (initial_percent < 0 || initial_percent > 1) {
    stop("Initial stage must be between 0 and 1.")
  }

  #res <- read.csv(ff,header=T)
  res <- rvn_res_read(ff)
  resdf <- res$res

  # initiate files
  fc2 <- file(output, open='w+a')
  writeLines(c('# Initial conditions for reservoirs in a Raven model',
               '# Copy + Paste these lines into the rvc file',
               '#'), fc2)

  #-- Regex Magic - get subbasin columns, ignore obs
  sb_cols <- grep('sub.[0-9]$',colnames(resdf), ignore.case = TRUE, )
  sb_id <- as.numeric(gsub('sub','',colnames(resdf)[sb_cols], ignore.case = TRUE))

  for (i in 1:length(sb_cols)) {
    stage <- as.numeric(resdf[,sb_cols[i]])
    value <- min(stage) + initial_percent * (max(stage) - min(stage))
    writeLines(sprintf(':InitialReservoirStage  %i  %.2f',sb_id[i], value), fc2)
  }
  close(fc2)

  return(TRUE)
}
