#' Number of Days in Month
#'
#' num.days.month is used to calculate the number of days in the month; handles
#' leap years
#'
#' @param date object in date format
#' @return \item{int}{number of days in the month}
#' @seealso \code{\link{num.days}} for calculating the number of days between
#' two dates
#'
#' See original code on post in Stack Overflow
#' \href{http://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-rfind}{
#' the number of days in a month}
#' @keywords number days month
#' @examples
#'
#' num.days.month(as.Date("2016-02-05"))
#' # 29
#'
#' num.days.month(as.Date("2017-01-17"))
#' # 31
#'
#' @export num.days.month
num.days.month <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}
