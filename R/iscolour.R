#' Check Validity of Colour Representation
#'
#' iscolour checks whether a string or string vector contains valid colour representations
#' (in text or hexadecimal form). Useful in error checking colour arguments for functions,
#'  such as SBMap.plot.
#'
#' @param x string or string vector of colour representations to test
#' @return \item{y}{vector of TRUE or FALSE indicating whether the colour is valid}
#' @seealso See original code on post in Stack Overflow
#' \href{https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa}{
#' Check if character string is a valid color representation}
#' @seealso \code{\link{col.transparent}} for creating transparent colour codes
#' @keywords colour check valid
#' @examples
#'
#'iscolour(c(NA, "black", "blackk", "1", "#00", "#000000"))
#'#   <NA>   black  blackk       1     #00 #000000
#'#   TRUE    TRUE   FALSE    TRUE   FALSE    TRUE
#'
#' @export iscolour
iscolour <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

