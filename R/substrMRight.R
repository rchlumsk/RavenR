#' substring minus characters from the Right
#'
#' substrMRight returns a string x with n characters removed from the right
#' side of the string.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the right side of the string
#' @seealso \code{\link{substrRight}} for using n characters from the right
#' side of string,
#'
#' \code{\link{substrLeft}} for using n characters from the left side of
#' string,
#'
#' \code{\link{substrMLeft}} for removing n characters from the left side of a
#' string,
#' @keywords string right minus
#' @examples
#'
#' substrRLeft("hello world",3)
#' # returns "hello wo"
#'
#' @export substrMRight
substrMRight <- function(x, n){
  substr(x, 1,nchar(x)-n)
}
