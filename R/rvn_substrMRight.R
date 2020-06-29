#' substring minus characters from the Right
#'
#' rvn_substrMRight returns a string x with n characters removed from the right
#' side of the string.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the right side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from the right
#' side of string,
#'
#' \code{\link{rvn_substrLeft}} for using n characters from the left side of
#' string,
#'
#' \code{\link{rvn_substrMLeft}} for removing n characters from the left side of a
#' string,
#' @keywords string right minus
#' @examples
#'
#' rvn_substrRLeft("hello world",3)
#' # returns "hello wo"
#'
#' @export rvn_substrMRight
rvn_substrMRight <- function(x, n){
  substr(x, 1,nchar(x)-n)
}
