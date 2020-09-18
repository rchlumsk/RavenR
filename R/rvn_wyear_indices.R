#' Water Year Indices
#'
#' rvn_wyear_indices returns the indices of the provided time series for the
#' start/end of the water year. The month/day of the water year defaults to October 1, but
#' may be supplied as other values, for example as July 1 in the Australian water year.
#' This functon is useful in supplying endpoints for water year evaluations.
#'
#' This function will return the indices for the specified date. Start and end indices representing partial
#' periods are not supplied.
#'
#' The sim and obs should be of time series (xts) format.
#'
#' @param sim xts object or Date/POSITXtc series to obtain indices for
#' @param mm month of water year (default 10)
#' @param dd day of water year (default 1)
#' @param force.ends whether to include series endpoints (default F)
#' @return \item{wyear.ind}{array of indices corresponding to start/end of
#' water years}
#'
#' @seealso \code{\link{apply_wyearly}} to apply functions over the water year#'
#' \code{\link{rvn_flow_scatterplot}} for creating flow scatterplots
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven water year indices
#' @examples
#'
#' # read in sample forcings data
#' data(rvn_forcing_data)
#'
#' # get the indices of the start of the water year for October 1
#' rvn_wyear_indices(rvn_forcing_data$forcings)
#' rvn_wyear_indices(rvn_forcing_data$forcings) %>%
#' rvn_forcing_data$forcings[., 1:2]
#'
#' # get the indices with endpoints
#' rvn_wyear_indices(rvn_forcing_data$forcings,force.ends=T) %>%
#' rvn_forcing_data$forcings[.,1:2]
#'
#' # get the indices of the start of the water year for July 1
#' rvn_wyear_indices(rvn_forcing_data$forcings, mm=7) %>%
#' rvn_forcing_data$forcings[., 1:2]
#'
#' @export rvn_wyear_indices
#' @importFrom lubridate month day
rvn_wyear_indices <- function(sim,mm=10,dd=1,force.ends=F)
{

  if (is.null(dim(sim))) {
    ep <- which(month(sim) == mm & day(sim) == dd)
    if (force.ends) {
      ep <- unique(c(1,ep,length(sim)))
    }
  } else if (dim(sim)[2] >= 1) {
    ep <- which(month(sim) == mm & day(sim) == dd)
    if (force.ends) {
      ep <- unique(c(1,ep,nrow(sim)))
    }
  } else {
    return(F)
  }

  return(ep)
}
