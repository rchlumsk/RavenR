#' Number of Days between two dates
#'
#' rvn_num_days is used to calculate the number of days in the month; handles leap
#' years
#'
#'
#' @param date1 first day, date format
#' @param date2 second day, date format
#' @return \item{int}{number of days between the two days}
#' @seealso \code{\link{rvn_num_days.month}} for calculating the number of days in a
#' month
#' @keywords days number
#' @examples
#'
#' rvn_num_days(as.Date("2017-02-05"),as.Date("2017-02-12"))
#' # 7
#'
#' @export rvn_num_days
rvn_num_days <- function(date1,date2) {
  return( length(seq.Date(from=date1,to=date2,by=1))-1 )
}
