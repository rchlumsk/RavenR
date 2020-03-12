#' Water Year Indices
#'
#' wyear.indices returns the indices of the provided time series for the
#' start/end of the water year (hardcoded as October 1st). Useful in evaluating
#' the time series in various diagnostics.
#'
#' This function will return the indices for October 1st for each year in the
#' time series object. If the series ends on a September 30th date, it will
#' return the index for September 30th. If it starts or ends on a date that is
#' not Oct 1st or Sept 30th, that portion of the data will be disregarded
#' (index not return for that period).
#'
#' The sim and obs should be of time series (xts) format.
#'
#' @param sim time series object to obtain indices for
#' @return \item{wyear.ind}{array of indices corresponding to start/end of
#' water years}
#' @seealso \code{\link{wyear.indices.aus}} for Australian water year
#'
#' \code{\link{flow.scatterplot}} for creating flow scatterplots
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven water year indices
#' @examples
#'
#' # read in sample forcings data
#' data("forcing.data")
#' fdata <- forcing.data$forcings
#'
#' # get the indices of the start of the water year
#' wyear.indices(fdata[,1])
#'
#' # [1]    1 366 731
#'
#' @export wyear.indices
wyear.indices <- function(sim) {
  temp <- sim[((month(sim[,1]) == 10) & (day(sim[,1]) == 1)) | ((month(sim[,1]) == 9) & (day(sim[,1]) == 30))]
  ep <- match(lubridate::date(temp),lubridate::date(sim))
  # remove all the unneccesary Sept 30th indices
  ind <- rep(0,length(ep))
  for (k in 1:(length(ep)-1)) {
    if ((ep[k+1]-ep[k])==1) {
      ind[k]=1
    }
  }
  ep <- cbind(ep,ind)[ind==0,1]
  return("wyear.ind"=ep)
}
