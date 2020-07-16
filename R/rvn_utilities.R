#' @title substring from the Left
#'
#' rvn_substrLeft returns n characters from the left side of the supplied string x.
#'
#' @param x a string to manipulate
#' @param n number of characters to use from the left side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from right side of
#' string,
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of
#' a string,
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


#' @title substring minus characters from the Left
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


#' @title substring minus characters from the Right
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


#' @title substring from the Right
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


#' @title Add Transparency to Colours
#'
#' rvn_col_transparent is used to adjust colour codes to introduce transparency
#'
#' Note that this function is not required for ggplot objects, as transparency can be added with the `alpha` parameter.
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
#' mycol <- rvn_col_transparent('blue',100)
#'
#' # plot more random points in transparent blue colour
#' points(rnorm(20),col=mycol)
#'
#' @export rvn_col_transparent
rvn_col_transparent <- function(colour,trans)
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


#' @title Check Validity of Colour Representation
#'
#' rvn_iscolour checks whether a string or string vector contains valid colour representations
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
#'rvn_iscolour(c(NA, "black", "blackk", "1", "#00", "#000000"))
#'#   <NA>   black  blackk       1     #00 #000000
#'#   TRUE    TRUE   FALSE    TRUE   FALSE    TRUE
#'
#' @export rvn_iscolour
rvn_iscolour <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}


#' @title Months in the Year vector
#'
#' rvn_mos_names is used to return a string vector of months in the year
#'
#' @param short boolean to return shortened form of months
#' @return \item{int}{number of days between the two days}
#' @seealso \code{\link{num.days.month}} for calculating the number of days in a
#' month
#' @keywords months year
#' @examples
#'
#' months_of_the_year <- rvn_mos_names
#' months_of_the_year
#'
#' rvn_mos_names(F)
#'
#' @export rvn_mos_names
rvn_mos_names <- function(short=T) {
  if (short) {
    return(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  } else {
    return(c('January','February','March','April','May','June','July',
             'August','September','October','November','December'))
  }
}


#' @title Number of Days between two dates
#'
#' rvn_num_days is used to calculate the number of days in the month; handles leap
#' years
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

  # update with this syntax?
  # as.numeric(difftime(index(date1) ,index(date2) , units = c("days")))

  return( length(seq.Date(from=date1,to=date2,by=1))-1 )
}


#' @title Number of Days in Month
#'
#' rvn_num_days_month is used to calculate the number of days in the month; handles
#' leap years
#'
#' @param date object in date format
#' @return \item{int}{number of days in the month}
#' @seealso \code{\link{rvn_num_days}} for calculating the number of days between
#' two dates
#'
#' See original code on post in Stack Overflow
#' \href{http://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-rfind}{
#' the number of days in a month}
#' @keywords number days month
#' @examples
#'
#' rvn_num_days_month(as.Date("2016-02-05"))
#' # 29
#'
#' rvn_num_days_month(as.Date("2017-01-17"))
#' # 31
#'
#' @export rvn_num_days_month
rvn_num_days_month <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}


#' @title Which Max (xts)
#'
#' rvn_which_max_xts applies the which.max function and returns an xts object
#' with the maximum value and associated date.
#'
#' This function is intended to act as the which.max function, applicable
#' to xts objects and returning values in an xts format.
#'
#' @param x xts object to apply which.max to
#' @return \item{x[which.max(x)]}{returned object}
#' @seealso \code{\link{which.max}} base which.max function
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords which max xts
#' @examples
#' data(rvn_hydrograph_data)
#' rvn_which_max_xts(rvn_hydrograph_data$hyd$Sub43_obs)
#'
#' @export rvn_which_max_xts
rvn_which_max_xts <- function(x) {
  return(x[which.max(x)])
}
