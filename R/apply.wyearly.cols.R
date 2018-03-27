#' Apply function for water year
#'
#' apply.wyearly.cols is a wrapper for apply.wyearly, and can be applied to
#' multiple columns of an xts data frame simultaneously.
#'
#' apply.wyearly.cols calculates a function FUN for the periods defined by the
#' water year, for each column in x. This similar to other functions of the
#' form apply.<time period>, for example apply.daily, apply.monthly, etc. This
#' is a function especially helpful to hydrology data or results.
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
#' @param x xts data frame to calculate FUN for
#' @param FUN the function to be applied
#' @param ... optional arguments to FUN
#' @seealso \code{\link{apply.wyearly}} for applying this function to a single
#' vector for most functions
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/software.html}{James R.
#' Craig's research page} for software downloads
#' @keywords water year apply
#' @examples
#'
#' # read in forcing data (use sample data from package0
#' data(forcing.data)
#' myforcings <- forcing.data
#'
#' # apply max function for a subset of forcings data; apply to multuple columns
#' apply.wyearly.cols(myforcings$forcings[,2:6],max,na.rm=T)
#'
#' @export apply.wyearly.cols
apply.wyearly.cols <- function(x,FUN,...) {

  if (missing(x)) {
    stop("Requires x object")
  }
  temp <- apply.wyearly(x[,1],FUN,...)
  # temp <- apply.wyearly(x[,1],sum)

  N <- nrow(temp)
  M <- ncol(x)
  dates <- temp[,1]
  mm <- matrix(NA,ncol=M,nrow=N)
  for (i in 1:M) {
    mm[,i] <- apply.wyearly(x[,i],FUN,...)[,2]
    # mm[,i] <- apply.wyearly(x[,i],sum)[,2]
  }
  ret <- data.frame(dates,mm)
  colnames(ret) <- c('date.end',colnames(x))
  return(ret)
}
