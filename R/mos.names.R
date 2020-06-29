#' Months in the Year vector
#'
#' mos.names is used to return a string vector of months in the year
#'
#' @param short boolean to return shortened form of months
#' @return \item{int}{number of days between the two days}
#' @seealso \code{\link{num.days.month}} for calculating the number of days in a
#' month
#' @keywords months year
#' @examples
#'
#' months_of_the_year <- mos.names
#' months_of_the_year
#'
#' mos.names(F)
#'
#' @export mos.names
mos.names <- function(short=T) {
  if (short) {
    return(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  } else {
    return(c('January','February','March','April','May','June','July',
             'August','September','October','November','December'))
  }
}

