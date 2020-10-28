#' @title Apply function for water year
#'
#' @description
#' rvn_apply_wyearly calculates a function FUN for the periods defined by the water
#' year, similar to other functions of the form apply.<time period>, for
#' example apply.daily, apply.monthly, etc.
#'
#' @details
#' This is a function especially helpful to hydrology data or results. The default water year start is
#' October 1st, but may be adjusted with the mm and dd arguments. The values for
#' mm and dd indicate the end of the water year period (i.e. mm=9 and dd=30 indicates
#' a new water year on Oct 1).
#'
#' @param x xts vector to calculate FUN for
#' @param FUN the function to be applied
#' @param ... optional arguments to FUN
#' @param mm month of water year ending (default 9)
#' @param dd day of water year ending (default 30)
#' @seealso \code{\link{rvn_wyear_indices}} for obtaining endpoints in the water year
#'
#' @examples
#' # use sample forcing data (or use forcings_read to read in ForcingFunctions.csv)
#' data(rvn_forcing_data)
#'
#' # apply mean as FUN to daily average temperature
#' rvn_apply_wyearly(rvn_forcing_data$forcings$temp_daily_ave,mean,na.rm=TRUE)
#'
#' # apply mean as FUN to all forcings
#' rvn_apply_wyearly(rvn_forcing_data$forcings,mean,na.rm=TRUE)
#'
#' # apply maximum via RavenR::cmax as FUN to all forcings (takes the max in each column)
#' ## note that the base::max will not work properly here
#' rvn_apply_wyearly(rvn_forcing_data$forcings,cmax,na.rm=TRUE)
#'
#' # apply to Australian water year (July 1)
#' rvn_apply_wyearly(rvn_forcing_data$forcings,cmax,na.rm=TRUE, mm=6, dd=30)
#'
#'
#' @export rvn_apply_wyearly
#' @importFrom xts period.apply
rvn_apply_wyearly <- function(x,FUN, ..., mm=9,dd=30) {
  ep <- rvn_wyear_indices(x, mm=mm, dd=dd)
  return(period.apply(x, ep, FUN, ...))
}
