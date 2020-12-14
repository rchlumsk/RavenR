#' @title Infill discontinuous time series with blank values
#'
#' @description
#' Infills missing time values from a time series based on a regular interval.
#'
#' @details
#' Takes xts dataset, finds minimum interval between time stamps and
#' returns a new regular interval xts with same data content, but NA values inbetween known
#' data values
#'
#' Only handles data with minimum time interval of 1 day; 1,2,3,4,6,8, or 12 hrs.
#'
#' Note that in default reading in of date/time data, the daylight savings timezones may be assigned
#' to the date/time when reading in a data file with Raven using functions such as \code{\link{rvn_hyd_read}}. This
#' function will then detect differences in the intervals and throw an error. To avoid this, the
#' timezone may be assigned explicitly to all values with the read function and all daylight savings/endings
#' will be ignored.
#'
#' @param ts valid xts time series
#'
#' @return {ts}{ continuous xts time series}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @examples
#' system.file("extdata","run1_Hydrographs.csv", package="RavenR") %>%
#' rvn_hyd_read(., tzone="EST") -> mydata
#' mydata <- mydata$hyd$precip
#' mydata<-mydata[-seq(2,nrow(mydata),3),] # remove every 3rd day
#' head(mydata)
#'
#' # fill back with rvn_ts_infill using NA values
#' rvn_ts_infill(mydata$precip) %>%
#' head()
#'
#' @export rvn_ts_infill
#' @importFrom zoo index
#' @importFrom xts xts
rvn_ts_infill <- function(ts)
{

  # find intervals
  ints<-as.numeric(difftime(index(ts[2:length(ts),]), index(ts[1:length(ts)-1,]) , units = c("days")))

  min_interval<-min(ints)
  max_interval<-max(ints)

  if (abs(min_interval-max_interval) > 1e-6){ # handle rounding error

    dates <- as.POSIXct(index(ts))

    if      (abs(min_interval-1/24)<0.001){byflag="hour"}
    else if (abs(min_interval-1.0 )<0.001){byflag="day"}
    else if (abs(min_interval-1/12)<0.001){byflag="2 hour"}
    else if (abs(min_interval-1/6 )<0.001){byflag="4 hour"}
    else if (abs(min_interval-1/4 )<0.001){byflag="6 hour"}
    else if (abs(min_interval-1/3 )<0.001){byflag="8 hour"}
    else if (abs(min_interval-1/2 )<0.001){byflag="12 hour"}
    else{
      print("Only able to handle hourly, daily, 2/4/6/8/12 hr xts time series")
      print(c('Minimum time step (hours) found in time series: ', min_interval))
      return (NA)
    }

    all.times <- xts(order.by=seq(dates[1], dates[length(ts)], by=byflag))

    # Merge - missing values are NA
    out <- merge(all.times, ts, all=TRUE)
    return (out)
  }
  return (ts)
}
