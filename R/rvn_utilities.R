#' @title substring from the Left
#'
#' @description rvn_substrLeft returns n characters from the left side of the supplied string x.
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the left side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from right side of string
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of a string
#'
#' @examples
#'
#' rvn_substrLeft("hello world",3)
#' # returns "hel"
#'
#' @export rvn_substrLeft
rvn_substrLeft <- function(x, n)
{
  substr(x, 1,n)
}


#' @title substring minus characters from the Left
#'
#' @description
#' rvn_substrMLeft returns a string x with n characters removed from the left side
#' of the string.
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the left side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from the right
#' side of string,
#'
#' \code{\link{rvn_substrLeft}} for using n characters from the left side of
#' string
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of
#' a string
#' @examples
#'
#' rvn_substrMLeft("hello world",3)
#' # returns "lo world"
#'
#' @export rvn_substrMLeft
rvn_substrMLeft <- function(x, n)
{
  substr(x, n+1,nchar(x))
}


#' @title substring minus characters from the Right
#'
#' @description
#' rvn_substrMRight returns a string x with n characters removed from the right
#' side of the string.
#'
#' @param x a string to manipulate
#' @param n number of characters to remove from the right side of the string
#' @seealso \code{\link{rvn_substrRight}} for using n characters from the right
#' side of string
#'
#' \code{\link{rvn_substrLeft}} for using n characters from the left side of
#' string
#'
#' \code{\link{rvn_substrMLeft}} for removing n characters from the left side of a
#' string
#' @examples
#'
#' rvn_substrMRight("hello world",3)
#' # returns "hello wo"
#'
#' @export rvn_substrMRight
rvn_substrMRight <- function(x, n)
{
  substr(x, 1,nchar(x)-n)
}


#' @title substring from the Right
#'
#' @description
#' rvn_substrRight returns n characters from the right side of the supplied string
#' x.
#'
#' @param x a string to manipulate
#' @param n number of characters to use from the right side of the string
#' @seealso \code{\link{rvn_substrLeft}} for using n characters from the left side
#' of string
#'
#' \code{\link{rvn_substrMRight}} for removing n characters from the right side of
#' a string
#'
#' \code{\link{rvn_substrMLeft}} for removing n characters from the left side of a
#' string
#' @examples
#'
#' rvn_substrRight("hello world",3)
#' # returns "rld"
#'
#' @export rvn_substrRight
rvn_substrRight <- function(x, n)
{
  substr(x, nchar(x)-n+1, nchar(x))
}


#' @title Add Transparency to Colours
#'
#' @description
#' rvn_col_transparent is used to adjust colour codes to introduce transparency
#'
#' @details
#' Note that this function is not required for ggplot objects, as transparency can be
#' added with the `alpha` parameter.
#'
#' @param colour time series containing columns you wish to reseasonalize. xts
#' object
#' @param trans integer describing the degree of transparency, from ~200
#' (slightly transparent) to <10 (very transparent)
#' @return \item{res}{returned updated colour code with transparency}
#'
#' @seealso See original code on post in Stack Overflow
#' \href{http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-rmaking}{
#' plot points transparent in R}
#' @seealso \code{\link{rvn_iscolour}} for checking validity of colour codes
#' @examples
#'
#' # plot randomly distributed data
#' plot(rnorm(20),col='black')
#'
#' # create a transparent blue colour for plotting
#' mycol <- rvn_col_transparent('red',100)
#'
#' # plot more random points in transparent red colour
#' points(rnorm(20),col=mycol)
#'
#' @export rvn_col_transparent
#' @importFrom grDevices col2rgb
rvn_col_transparent <- function(colour,trans)
{
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
#' @description
#' rvn_iscolour checks whether a string or string vector contains valid colour representations
#' (in text or hexadecimal form). Useful in error checking colour arguments for functions.
#'
#' @param x string or string vector of colour representations to test
#' @return \item{y}{vector of TRUE or FALSE indicating whether the colour is valid}
#'
#' @seealso See original code on post in Stack Overflow
#' \href{https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa}{
#' Check if character string is a valid color representation}
#' @seealso \code{\link{rvn_col_transparent}} for creating transparent colour codes
#' @examples
#'
#' rvn_iscolour(c(NA, "black", "blackk", "1", "#00", "#000000"))
#' #   <NA>   black  blackk       1     #00 #000000
#' #   TRUE    TRUE   FALSE    TRUE   FALSE    TRUE
#'
#' @export rvn_iscolour
rvn_iscolour <- function(x)
{
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}


#' @title Months in the Year vector
#'
#' @description
#' rvn_month_names is used to return a character vector of months in the year
#'
#' @param short boolean to return shortened form of months
#' @return {character array of month names}
#'
#' @seealso \code{\link{rvn_num_days}} for calculating the number of days in a
#' month
#' @examples
#' rvn_month_names()
#' rvn_month_names(FALSE)
#'
#' @export rvn_month_names
rvn_month_names <- function(short=TRUE)
{
  if (short) {
    return(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  } else {
    return(c('January','February','March','April','May','June','July',
             'August','September','October','November','December'))
  }
}


#' @title Number of Days between two dates
#'
#' @description
#' rvn_num_days is used to calculate the number of days between two provided dates.
#'
#' @details
#' This method handles leap years if they exist between the specified dates.
#'
#' @param date1 first day, date format
#' @param date2 second day, date format
#' @return \item{int}{number of days between the two days}
#'
#' @seealso \code{\link{rvn_num_days_month}} for calculating the number of days in a
#' month
#'
#' @examples
#' rvn_num_days(as.Date("2017-02-05"),as.Date("2017-02-12"))
#' # 7
#'
#' @export rvn_num_days
rvn_num_days <- function(date1,date2)
{
  # alternate method
  # as.numeric(difftime(index(date1) ,index(date2) , units = c("days")))

  return( length(seq.Date(from=date1,to=date2,by=1))-1 )
}


#' @title Number of Days in Month
#'
#' @description
#' rvn_num_days_month is used to calculate the number of days in the month
#'
#' @details
#' This method includes leap years if they exist in the specified month.
#'
#' @param date object in date format
#' @return \item{int}{number of days in the month}
#' @seealso \code{\link{rvn_num_days}} for calculating the number of days between
#' two dates
#'
#' See original code on post in Stack Overflow
#' \href{http://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-rfind}{
#' the number of days in a month}
#' @examples
#'
#' rvn_num_days_month(as.Date("2016-02-05"))
#' # 29
#'
#' rvn_num_days_month(as.Date("2017-01-17"))
#' # 31
#'
#' @export rvn_num_days_month
rvn_num_days_month <- function(date)
{
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

#' @title cmax
#'
#' @description
#' Applies the base::max function across columns.
#'
#' @details
#' It applies the base::max function over
#' columns, which is advantageous for calculating the max within a column
#' rather than the max of the whole data frame. The default base::max will not work
#' properly for data frames and other structures in applying over columns or different periods.
#'
#' This function was included for usage with the apply.<period> and rvn_apply_wyearly
#' function, as the base::max function does not work properly across columns.
#'
#' @param x object to apply the max function to
#' @param na.rm whether to remove na values from the calculation
#'
#' @return x with the max value in each column determined
#'
#' @seealso \code{\link{rvn_apply_wyearly}} where this function can be applied for the water year,
#' and the xts functions such as \code{\link{apply.yearly}} and \code{\link{apply.monthly}}
#'
#' @examples
#' data(rvn_hydrograph_data)
#' cmax(rvn_hydrograph_data$hyd$Sub43_obs, na.rm=TRUE)
#'
#' rvn_apply_wyearly(rvn_hydrograph_data$hyd, cmax, na.rm=TRUE)
#'
#' @export cmax
cmax <- function(x, na.rm = FALSE)
{
  apply(x, 2, max, na.rm = na.rm)
}

#' @title which.max for xts objects
#'
#' @description
#' rvn_which_max_xts applies the which.max function and returns an xts object
#' with the maximum value and associated date.
#'
#' @details
#' This function is intended to act as the which.max function, applicable
#' to xts objects and returning values in an xts format.
#'
#' Note that when deploying the rvn_apply_wyearly function, the dates are overwritten
#' and the dates of the water year ending periods are displayed rather than the event dates.
#' In order to obtain the corresponding dates when using the rvn_apply_wyearly function, please use
#' \code{\link{rvn_apply_wyearly_which_max_xts}}.
#'
#' @param x xts object to apply which.max to
#' @return {xts object with max value and corresponding date}
#' @seealso \code{\link{which.max}} base which.max function
#' \code{\link{rvn_apply_wyearly_which_max_xts}} for using apply_wyearly with the rvn_which_max_xts function
#'
#' @examples
#' data(rvn_hydrograph_data)
#'
#' # obtain the peak observed flow and the corresponding date
#' rvn_which_max_xts(rvn_hydrograph_data$hyd$Sub43_obs)
#'
#' # note that the usual rvn_apply_wyearly does not provide the correct dates with this function
#' rvn_apply_wyearly(rvn_hydrograph_data$hyd$Sub43_obs, rvn_which_max_xts)
#'
#' @export rvn_which_max_xts
#' @importFrom lubridate date
rvn_which_max_xts <- function(x)
{
  if ("xts" %in% class(x) & ncol(x) == 1 ) {

    df <- data.frame(date=lubridate::date(x)[which.max(x)],max=as.numeric(x[which.max(x)]))
    myxts <- xts(df$max, order.by=df$date)
    colnames(myxts) <- colnames(x)
    return(myxts)

  } else {
    warning("x must be an xts object with one numeric column")
    return(FALSE)
  }
}



#' @title which.max over water year periods
#'
#' @description
#' rvn_apply_wyearly_which_max_xts applies the which.max function within each
#' water year period, and returns the corresponding max values and dates in an xts format.
#'
#' @param x xts object
#' @param mm month of water year ending (default 9)
#' @param dd day of water year (default 30)
#'
#' @return {xts object with max values and corresponding dates}
#'
#' @examples
#' data(rvn_hydrograph_data)
#'
#' # obtain peak observed flows in each water year period
#' rvn_apply_wyearly_which_max_xts(rvn_hydrograph_data$hyd$Sub43_obs)
#'
#' # will return a warning with no result if multiple columns supplied
#' rvn_apply_wyearly_which_max_xts(rvn_hydrograph_data$hyd)
#'
#' @export rvn_apply_wyearly_which_max_xts
#' @importFrom lubridate date
#' @importFrom xts xts
rvn_apply_wyearly_which_max_xts <- function(x, mm=9, dd=30)
{
  # return(x[which.max(x)])
  if ("xts" %in% class(x) & ncol(x) == 1 ) {
    ep <- rvn_wyear_indices(x,mm=mm,dd=dd)
    dx <- as.Date(rep(NA,length(ep)-1))
    xx <- rep(NA,length(ep)-1)

    for (i in 1:(length(ep)-1)) {
      dx[i] <- lubridate::date(x[ep[i]:ep[i+1]])[which.max(x[ep[i]:ep[i+1]])]
      xx[i] <- as.numeric(x[ep[i]:ep[i+1]][which.max(x[ep[i]:ep[i+1]])])
    }

    myxts <- xts(xx, order.by=dx)
    colnames(myxts) <- colnames(x)
    return(myxts)

  } else {
    warning("x must be an xts object with one numeric column")
    return(FALSE)
  }
}

#' @title Check period input
#'
#' @description
#' rvn_get_prd is a robust function used to check a period argument either as
#' a character or against an xts object.
#'
#' @details
#' The function may take some combination of an xts object, a character string or both.
#'
#' If a character is provided, the consistency of the character string against the YYYY-MM-DD/YYYY-MM-DD format
#' is checked. If an xts object is provided, the period for that xts object is returned. If both
#' are provided to the function, both checks are made and the consistency of the character period
#' against the xts object is performed. In any case, a period character string is returned.
#'
#' @param x xts object
#' @param prd period argument in format YYYY-MM-DD/YYYY-MM-DD as a character
#' @return {prd argument with warnings provided if needed}
#' @seealso \code{\link{rvn_theme_RavenR}} provides a theme for the RavenR package
#'
#' @examples
#' data(rvn_hydrograph_data)
#'
#' # check if string is a valid prd argument
#' rvn_get_prd(prd="2000-10-01/2002-09-30")
#' # rvn_get_prd(prd="2000-10-01/2002-24-30") # returns error
#'
#' # get full valid prd argument for xts object
#' rvn_get_prd(rvn_hydrograph_data$hyd$Sub43_obs)
#'
#' # check prd argument against xts object
#' rvn_get_prd(rvn_hydrograph_data$hyd$Sub43_obs, "2020-01-01/2020-02-01")
#' # rvn_get_prd(rvn_hydrograph_data$hyd$Sub43_obs, "2002-24-01/2020-02-01") # returns error
#' # rvn_get_prd(rvn_hydrograph_data$hyd$Sub43_obs, "20-24-01/2020-02-01")   # returns error
#'
#'
#' @export rvn_get_prd
#' @importFrom lubridate date year month day
rvn_get_prd <- function(x=NULL, prd=NULL)
{
# determine the period to use
  if (!(is.null(prd))) {

    # prd is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period argument prd; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period argument prd; two dates should be in YYYY-MM-DD format.")
    }
    # add conversion to date with xts format check ?

    # check that both periods are valid dates
   tryCatch(
      {
        as.Date(unlist(strsplit(prd,"/"))[1])
      },
      error=function(cond) {
        stop(paste("Issue with prd, %s does not appear to be a valid date in Y-M-D format:",
                      unlist(strsplit(prd,"/"))[1]))
        message(cond)
        # return(FALSE)
        stop()
      },
      warning=function(cond) {
        message(paste("Warning with prd, %s does not appear to be a valid date in Y-M-D format:",
                      unlist(strsplit(prd,"/"))[1]))
        message(cond)
      }
    )

    tryCatch(
      {
        as.Date(unlist(strsplit(prd,"/"))[2])
      },
      error=function(cond) {
        stop(paste("Issue with prd, %s does not appear to be a valid date in Y-M-D format:",
                      unlist(strsplit(prd,"/"))[2]))
        message(cond)
        # return(FALSE)
      },
      warning=function(cond) {
        message(paste("Warning with prd, %s does not appear to be a valid date in Y-M-D format:",
                      unlist(strsplit(prd,"/"))[2]))
        message(cond)
      }
    )

    if (!(is.null(x))) {
      # check that the prd is valid for x
      if (nrow(x[prd]) == 0) {
        warning("x is empty for supplied prd argument")
      }
    }

  } else {
    # period is not supplied

    if (!(is.null(x))) {
      # get the whole range
      N <- nrow(x)
      prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(x[1,1]),month(x[1,1]),day(x[1,1]),
                     year(x[N,1]),month(x[N,1]),day(x[N,1]) )
    }
  }
  return(prd)
}


#' @title Pads string with spaces, either right or left justified
#'
#' @description
#' Pad string with spaces, justified on either the left or right
#'
#' @param string Text string
#' @param width Number of characters total, including desired spaces
#' @param just 'r' for right, 'l' for left
#'
#' @return {Padded string}
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#' @examples
#' # Returns "   To the right"
#' rvn_stringpad('To the right', 15, just='r')
#'
#' # Returns "Padded    "
#' rvn_stringpad('Padded', 10, just='l')
#' @export rvn_stringpad
rvn_stringpad <- function(string, width, just='r')
{
  slength <- nchar(string)
  padlength <- width - slength
  #-- Break if string is longer than the pad width
  if (padlength < 0) {
    stop('String exceeds total width')
  }
  #-- return a padded string
  if (just == 'r') {
    return(paste0(strrep(' ', padlength), string))
  }
  else if (just == 'l') {
    return(paste0(string, strrep(' ', padlength)))
  }
}
