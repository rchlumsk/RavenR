#' substring from the Right
#'
#' substrRight returns n characters from the right side of the supplied string
#' x.
#'
#'
#' @param x a string to manipulate
#' @param n number of characters to use from the right side of the string
#' @seealso \code{\link{substrLeft}} for using n characters from the left side
#' of string,
#'
#' \code{\link{substrMRight}} for removing n characters from the right side of
#' a string,
#'
#' \code{\link{substrMLeft}} for removing n characters from the left side of a
#' string
#' @keywords string right
#' @examples
#'
#' substrRight("hello world",3)
#' # returns "rld"
#'
#' @export substrRight
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
