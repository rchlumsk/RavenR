#' Add Transparency to Colours
#'
#' col.transparent is used to adjust colour codes to introduce transparency
#'
#'
#' @param colour time series containing columns you wish to reseasonalize. xts
#' object
#' @param trans integer describing the degree of transparency, from ~200
#' (slightly transparent) to <10 (very transparent)
#' @return \item{res}{returned updated colour code with transparency}
#' @seealso See original code on post in Stack Overflow
#' \href{http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-rmaking}{
#' plot points transparent in R}
#' @seealso \code{\link{iscolour}} for checking validity of colour codes
#' @keywords colour transparency
#' @examples
#'
#' # plot randomly distributed data
#' plot(rnorm(20),col='black')
#'
#' # create a transparent blue colour for plotting
#' mycol <- col.transparent('blue',100)
#'
#' # plot more random points in transparent blue colour
#' points(rnorm(20),col=mycol)
#'
#' @export col.transparent
col.transparent <- function(colour,trans)
{
  # This function adds transparancy to a colour.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either colour and trans a vector of equal length,
  # or one of the two of length 1.
  # from http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r

  if (length(colour)!=length(trans)&!any(c(length(colour),length(trans))==1)) stop("Vector lengths not correct")
  if (length(colour)==1 & length(trans)>1) colour <- rep(colour,length(trans))
  if (length(trans)==1 & length(colour)>1) trans <- rep(trans,length(colour))

  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(colour),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

