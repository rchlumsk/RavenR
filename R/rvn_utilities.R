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
#' Acts as the which.max function, applicable
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

#' @title \%notin\% operator
#'
#' @description
#' Syntax for the opposite of \%in\%
#'
#' @param x values to be matched
#' @param table the values to be matched against
#'
#' @examples
#' seq(1,5,1) %notin% seq(3,7,1)
#'
#' @export
"%notin%" <- function(x, table) match(x, table, nomatch = 0) == 0


#' @title Fortify xts object to specific format
#'
#' @description
#' Applies the fortify function to an xts object and updates the Index character
#' column to a date column called 'Date'.
#'
#' @details
#' Useful in preparing data to plotting or other tidy-style analysis.
#' This function is used internally in many RavenR plotting functions.
#'
#' @param x xts formatted object to fortify to tibble
#'
#' @return tibble format of the xts data
#'
#' @examples
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#' hyd <- rvn_hyd_read(ff)$hyd
#' hyd_fortified <- rvn_fortify_xts(hyd)
#' head(hyd_fortified)
#'
#' @export rvn_fortify_xts
#' @importFrom ggplot2 fortify
rvn_fortify_xts <- function(x)
{
  if ("xts" %notin% class(x)) {
    stop("x must be of class xts.")
  }

  y <- fortify(x)
  colnames(y) <- c("Date",colnames(y)[-1])
  y$Date <- as.Date(y$Date)
  return(y)
}


#' @title Provide mappings for rvt files
#'
#' @description
#' Provides global variables to the environment regarding the mapping of rvt_type and data_type
#' that are used in the \code{\link{rvn_rvt_write}} function.
#'
#'#' @details
#' Function is used to update the rvn_rvt_mappings.RData file in the RavenR data folder,
#' and should not be run by regular RavenR users. Most users should load the existing
#' data objects in the /data folder with the \code{data("rvn_rvt_mappings_data")} command.
#'
#' @return \code{TRUE} if executed successfully.
#'
#' @seealso \code{\link{rvn_rvt_write}}
#'
#' @examples
#' rvn_rvt_mappings()
#'
#' # view global variables
#' rvt_mapping
#' rvt_data_type_mapping
#'
#'#' @keywords internal
#' @export rvn_rvt_mappings
rvn_rvt_mappings <- function() {

  # generates globally accessible variables for rvt_mapping and rvt_data_type_mapping

  # update based on additional entries in the list
  rvt_mapping <<- list(

    "Data"=list(
      c("forcing_type","units"),
      c("start_datetime","time_interval","num_points")
    ),

    "MultiData"=list(
      c(NULL),
      c("start_datetime","time_interval","num_points"),
      c(":Parameters"),
      c(":Units")
    ),

    "ObservationData"=list(
      c("data_type","basin_ID","units"),
      c("start_datetime","time_interval","num_points")
    ),

    "ObservationWeights"=list(
      c("data_type","basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "IrregularObservations"=list(
      c("data_type","basin_ID","num_points","units")
    ),

    "IrregularWeights"=list(
      c("data_type","basin_ID","num_points")
    ),

    "ReservoirExtraction"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "VariableWeirHeight"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMaxStage"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMinStage"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMinStageFlow"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "OverrideReservoirFlow"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirTargetStage"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMinFlow"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMaxFlow"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMaxQDelta"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "ReservoirMaxQDecrease"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "BasinInflowHydrograph"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "BasinInflowHydrograph2"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "IrrigationDemand"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    ),

    "EnvironmentalMinFlow"=list(
      c("basin_ID"),
      c("start_datetime","time_interval","num_points")
    )
  )

  # update this based on table C.1 in Raven Manual?
  rvt_data_type_mapping <<- list(
    "HYDROGRAPH"=list(
      "units"="m3/s"
    ),
    "RESERVOIR_STAGE"=list(
      "units"="m"
    ),
    "RESERVOIR_INFLOW"=list(
      "units"="m3/s"
    ),
    "RESERVOIR_NETINFLOW"=list(
      "units"="m3/s"
    )
  )

  ## add TEMP_DAILY_MIN, TEMP_DAILY_MAX?
  rvn_met_raven_mapping <<- list(
    "PRECIP"=list("units"="mm/d"),
    "SNOW_FRAC"=list("units"="%"),
    "SNOWFALL"=list("units"="mm/d"),
    "RAINFALL"=list("units"="mm/d"),
    "TEMP_AVE"=list("units"="DegC"),
    "TEMP_MAX"=list("units"="DegC"),
    "TEMP_DAILY_MAX"=list("units"="DegC"),
    "TEMP_MIN"=list("units"="DegC"),
    "TEMP_DAILY_MIN"=list("units"="DegC"),
    "REL_HUMIDITY"=list("units"="%"),
    "ET_RADIA"=list("units"="MJ/m2/d"),
    "SHORTWAVE"=list("units"="MJ/m2/d"),
    "LW_INCOMING"=list("units"="MJ/m2/d"),
    "WIND_VEL"=list("units"="m/s"),
    "PET"=list("units"="mm/d"),
    "OW_PET"=list("units"="mm/d"),
    "POTENTIAL_MELT"=list("units"="mm/d")
  )

  # weathercan mapping to standard Raven names
  ## weathercan_name <-> Raven_name
  rvt_met_mapping_weathercan <<- list(
    "TOTAL_PRECIP"=list("PRECIP"),
    "TOTAL_RAIN"=list("RAINFALL"),
    "TOTAL_SNOW"=list("SNOWFALL"),
    "MAX_TEMP"=list("TEMP_MAX"),
    "MIN_TEMP"=list("TEMP_MIN"),
    "WIND_SPD"=list("WIND_VEL"), # warning on unit conversion
    "REL_HUM"=list("REL_HUMIDITY"),
    "PRECIP_AMT"=list("PRECIP"), # add warning on unit conversion
    "PRESSURE"=list("AIR_PRES")
  )

  # save to RavenR /data folder
  # save(rvn_met_raven_mapping,
  #      rvt_data_type_mapping,
  #      rvt_mapping,
  #      rvt_met_mapping_weathercan,
  #      file="data/rvn_rvt_mappings_data.RData")

  return(TRUE)
}


