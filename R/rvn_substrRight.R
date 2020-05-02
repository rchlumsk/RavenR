#' substring from the Right
#'
#' rvn_substrRight returns n characters from the right side of the supplied string
#' x.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to use from the right side of the string
#' @seealso \code{\link{rvn_substrLeft}} for using n characters from the left side
#' of string,
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of
#' a string,
#'
#' \code{\link{rvn_substrMLeft}} for removing n characters from the left side of a
#' string
#' @keywords string right
#' @examples
#'
#' rvn_substrRight("hello world",3)
#' # returns "rld"
#'
#' @export rvn_substrRight
rvn_substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
