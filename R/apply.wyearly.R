#' Apply function for water year
#'
#' apply.wyearly calculates a function FUN for the periods defined by the water
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
#' The apply.wyearly function applies to only one column at a time, otherwise
#' only the first column will be used, with a warning.
#'
#' @param x xts vector to calculate FUN for
#' @param FUN the function to be applied
#' @param ... optional arguments to FUN
#' @seealso \code{\link{apply.wyearly.cols}} for applying this function to
#' multiple columns explicitly
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/software.html}{James R.
#' Craig's research page} for software downloads
#' @keywords water year apply
#' @examples
#'
#' # use sample forcing data (or use forcings.read to read in ForcingFunctions.csv)
#' data(forcing.data)
#' myforcings <- forcing.data
#'
#' # apply mean as FUN to daily average temperature
#' apply.wyearly(myforcings$forcings$temp_daily_ave,mean,na.rm=T
#'
#' # use apply.wyearly.cols to apply to multiple columns at once
#' apply.wyearly.cols(myforcings$forcings[,2:6],max,na.rm=T)
#'
#' @export apply.wyearly
apply.wyearly <- function(x,FUN,...) {

  if (ncol(x) > 1) {
    warning("More than one column in x, only the first will be used.")
  }

  temp <- x[((month(x[,1]) == 10) & (day(x[,1]) == 1)) | ((month(x[,1]) == 9) & (day(x[,1]) == 30))]
  ep <- match(lubridate::date(temp),lubridate::date(x))

  # remove all the unneccesary Sept 30th indices
  ind <- rep(0,length(ep))
  for (i in 1:(length(ep)-1)) {
    if ((ep[i+1]-ep[i])==1) {
      ind[i]=1
    }
  }
  ep <- cbind(ep,ind)[ind==0,1]
  # create data frame to store results, including end dates of water years
  df <- data.frame("date.end"=lubridate::date(x[ep[2:length(ep)]]),"FUN"=rep(NA,length(ep)-1))
  # get results and apply functions
  for (i in 1:nrow(df)) {
    temp <- x[ep[i]:(ep[i+1]-1)]
    df[i,2] <- FUN(temp,...)
    # df[i,2] <- lubridate::date(which.max.xts(temp))
  }
  return(df)
}
