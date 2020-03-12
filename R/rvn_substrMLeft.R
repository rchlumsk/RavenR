#' substring minus characters from the Left
#'
#' rvn_substrMLeft returns a string x with n characters removed from the left side
#' of the string.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the left side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from the right
#' side of string,
#'
#' \code{\link{rvn_substrLeft}} for using n characters from the left side of
#' string,
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of
#' a string,
#' @keywords string left minus
#' @examples
#'
#' rvn_substrMLeft("hello world",3)
#' # returns "lo world"
#'
#' @export rvn_substrMLeft
rvn_substrMLeft <- function(x, n){
  substr(x, n+1,nchar(x))
}
