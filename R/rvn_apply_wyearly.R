#' Apply function for water year
#'
#' rvn_apply_wyearly calculates a function FUN for the periods defined by the water
#' year, similar to other functions of the form apply.<time period>, for
#' example apply.daily, apply.monthly, etc. This is a function especially
#' helpful to hydrology data or results. The assumed water year start is
#' October 1st.
#'
#' The function assumes an October 1st start to the water year. If the data
#' supplied is from, e.g. 2008-10-01 to 2009-09-30, the function will use the
#' September 30th date as the final day to apply the function for the water
#' year.
#'
#' Note that currently the water year calculation is done from Oct 2 to Oct 1st
#' (or September 30th) inclusive. For example in the Oct 1 2007 to Oct 1 2008
#' water year, the end date is shown as "2008-10-01", and the function will be
#' evaluated for the period 2007-10-02 - 2008-10-01, inclusive.
#'
#' The rvn_apply_wyearly function applies to only one column at a time, otherwise
#' only the first column will be used, with a warning.
#'
#' @param x xts vector to calculate FUN for
#' @param FUN the function to be applied
#' @param ... optional arguments to FUN
#' @seealso \code{\link{rvn_wyear_indices}} for obtaining endpoints in the water year
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/software.html}{James R.
#' Craig's research page} for software downloads
#' @keywords water year apply
#' @examples
#'
#' # use sample forcing data (or use forcings_read to read in ForcingFunctions.csv)
#' data(rvn_forcing_data)
#'
#' # apply mean as FUN to daily average temperature
#' rvn_apply_wyearly(rvn_forcing_data$forcings$temp_daily_ave,mean,na.rm=T)
#'
#' # apply max as FUN to all forcing data, with end points included for partial periods in each water year
#' ## note that this uses the cmax function in RavenR rather than the base::max function
#' rvn_apply_wyearly(rvn_forcing_data$forcings,cmax,na.rm=T)
#'
#' @export rvn_apply_wyearly
rvn_apply_wyearly <- function(x,FUN,...,force.ends=F) {
  ep <- rvn_wyear_indices(x, force.ends=force.ends)
  period.apply(x, ep, FUN, ...) %>%
    return(.)
}
