#' Whixh Max (xts)
#'
#' which.max.xts applies the which.max function and returns an xts object
#' with the maximum value and associated date.
#'
#' This function is intended to act as the which.max function, applicable
#' to xts objects and returning values in an xts format.
#'
#' @param x xts object to apply which.max to
#' @return \item{x[which.max(x)]}{returned object}
#' @seealso \code{\link{which.max}} base which.max function
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords which max xts
#' @examples
#' data(hydrograph.data)
#' which.max.xts(hydrograph.data$hyd$Sub43_obs)
#'
#' @export which.max.xts
which.max.xts <- function(x) {
  return(x[which.max(x)])
}
