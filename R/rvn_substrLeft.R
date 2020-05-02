#' substring from the Left
#'
#' rvn_substrLeft returns n characters from the left side of the supplied string x.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to use from the left side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from right side of
#' string,
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of
#' a string,
#'
#' \code{\link{rvn_substrMLeft}} for removing n characters from the left side of a
#' string
#' @keywords string left
#' @examples
#'
#' rvn_substrLeft("hello world",3)
#' # returns "hel"
#'
#' @export rvn_substrLeft
rvn_substrLeft <- function(x, n){
  substr(x, 1,n)
}
