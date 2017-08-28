#' substring minus characters from the Left
#'
#' substrMLeft returns a string x with n characters removed from the left side
#' of the string.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the left side of the string
#' @seealso \code{\link{substrRight}} for using n characters from the right
#' side of string,
#'
#' \code{\link{substrLeft}} for using n characters from the left side of
#' string,
#'
#' \code{\link{substrMRight}} for removing n characters from the right side of
#' a string,
#' @keywords string left minus
#' @examples
#'
#' substrMLeft("hello world",3)
#' # returns "lo world"
#'
#' @export substrMLeft
substrMLeft <- function(x, n){
  substr(x, n+1,nchar(x))
}
