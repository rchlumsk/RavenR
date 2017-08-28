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
#' # read in forcing data
#' data(forcing.data)
#' myforcings <- forcing.data
#' # myforcings <- forcings.read('model_output/ForcingFunctions.csv')
#'
#' # apply mean as FUN to multiple columns in a subset of the forcings
#' apply.wyearly(myforcings$forcings[,2:5],mean,na.rm=T)
#'
#' # 1 2008-10-01 2.026634 1.0132499 4.027819         -2.5779686
#' # 2 2009-10-01 1.824596 0.7682891 3.411714         -3.1898807
#' # 3 2010-10-01 2.134978 0.4816056 5.729606         -0.2589522
#' # 4 2011-10-01 1.710114 0.6616020 4.898457         -0.7726190
#' # 5 2012-10-01 1.962943 0.7989751 6.595685          0.5769710
#' # 6 2013-10-01 2.167356 0.8879008 5.151154         -0.4599722
#' # 7 2014-10-01 1.957373 0.6778523 3.286140         -2.6790306
#' # 8 2015-10-01 1.755466 0.6177513 3.720554         -2.6953915
#'
#' # note that this function will not work properly for max or min functions;
#' # for this use apply.wyearly.cols
#' apply.wyearly(myforcings$rav.obj[,2:6],max,na.rm=T)
#'
#' # date.end     fun
#' # 1 2008-10-01 42.3816
#' # 2 2009-10-01 32.3244
#' # 3 2010-10-01 33.5408
#' # 4 2011-10-01 34.1169
#' # 5 2012-10-01 33.2695
#' # 6 2013-10-01 39.0579
#' # 7 2014-10-01 36.2075
#' # 8 2015-10-01 32.2755
#'
#' @export apply.wyearly
apply.wyearly <- function(x,FUN,...) {
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
  res <- xts::period.apply(x, ep, FUN, ...)[2:length(ep)]
  # res <- period.apply(x, ep, sum)[2:length(ep)]

  return(data.frame("date.end"= (lubridate::date(res)),"fun"=coredata(res)))
}
