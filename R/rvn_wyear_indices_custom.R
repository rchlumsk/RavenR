#' Water Year Indices Custom
#'
#' wyear.indices.custom returns the indices of the provided time series for the
#' start/end of the water year, as defined by the user.
#'
#' This function will return the indices for start of the water year, as
#' defined by the user using the mm and dd inputs. Note that unlike the
#' hardcoded functions, the day before the start of the water year will not be
#' searched as well.
#'
#' The sim and obs should be of time series (xts) format.
#'
#' @param sim time series object to obtain indices for
#' @param mm integer of month defined as start of the water year
#' @param dd integer of day defined as start of of the water year
#' @return \item{wyear.ind}{array of indices corresponding to start/end of
#' water years}
#' @seealso \code{\link{wyear.indices.aus}} for Australian water year
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven water year indices custom
#' @examples
#'
#' # read in sample forcings data
#' data("forcing.data")
#' fdata <- forcing.data$forcings
#'
#' # get the indices of the start of the water year defined as April 1st
#' wyear.indices.custom(fdata[,1],4,1)
#'
#' @export wyear.indices.custom
wyear.indices.custom <- function(sim,mm,dd) {
  temp <- sim[((month(sim[,1]) == mm) & (day(sim[,1]) == dd))]
  ep <- match(lubridate::date(temp),lubridate::date(sim))
  return("wyear.ind"=ep)
}
