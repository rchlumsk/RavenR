#' @title Water Year Indices
#'
#' @description
#' rvn_wyear_indices returns the indices of the provided time series for the
#' start/end of the water year. The month/day of the water year defaults to September 30 for
#' an October 1 water year cycle. However, this may be supplied as other values,
#' for example as June 30th for a July 1 water year (i.e. the Australian water year).
#' This function is useful in supplying endpoints for water year evaluations.
#'
#' @details
#' Note that this function is meant to emulate the \code{\link{endpoints}} function
#' for a water year period. The first and last points are included in all supplied endpoints,
#' which may introduce partial periods to the analysis.#'
#'
#' @param x xts object or Date/POSITXtc series to obtain indices for
#' @param mm month of water year (default 9)
#' @param dd day of water year (default 30)
#' @return \item{ep}{array of indices corresponding to start/end of
#' water years}
#'
#' @seealso \code{\link{rvn_apply_wyearly}} to apply functions over the water year
#' \code{\link{endpoints}} workhorse function for generating endpoints in xts for other periods
#'
#' @examples
#'
#' # read in sample forcings data
#' data(rvn_forcing_data)
#'
#' # get the indices of the water year for October 1 (the default)
#' rvn_wyear_indices(rvn_forcing_data$forcings)
#' rvn_wyear_indices(rvn_forcing_data$forcings) %>%
#' rvn_forcing_data$forcings[., 1:2]
#'
#' # get the indices of the start of the water year for July 1
#' ## note that the last period is the last index, and not a complete water year period
#' rvn_wyear_indices(rvn_forcing_data$forcings, mm=6, dd=30) %>%
#' rvn_forcing_data$forcings[., 1:2]
#'
#' @export rvn_wyear_indices
#' @importFrom lubridate month day
rvn_wyear_indices <- function(x,mm=9,dd=30)
{
  if (is.null(dim(x))) {
    ep <- which(lubridate::month(x) == mm & lubridate::day(x) == dd)
    ep <- unique(c(0,ep,length(x)))
  } else if (dim(x)[2] >= 1) {
    ep <- which(lubridate::month(x) == mm & lubridate::day(x) == dd)
    ep <- unique(c(0,ep,nrow(x)))
  } else {
    return(FALSE)
  }
  return(ep)
}
