#' Infill discontinuous time series with blank values
#' 
#' Takes xts dataset, finds minimum interval between time stamps and 
#' returns a new regular interval xts with same data content, but NA values inbetween known 
#' data values
#' 
#' @param ts valid xts time series 
#' 
#' @details only handles data with minimum time interval of 1 day; 1,2,3,4,6,8, or 12 hrs
#'
#' @return continuous xts time series 
#' 
#' @author James R. Craig, University of Waterloo
#' 
#' @examples
#'   dates <-seq(as.POSIXct("2012-05-01 00:00:00"), length=731, by="day")
#'   mydata<-xts(rnorm(731),order.by=dates)
#'   mydata<-mydata[wday(index(mydata))!=4] # remove wednesdays
#'   out<-timeseries.infill(mydata)
#' 
#' @keywords timeseries infill 
#'
timeseries.infill<-function(ts)
{
  # find intervals
  ints<-as.numeric(difftime(index(ts[2:length(mydata),]) ,index(ts[1:length(mydata)-1,]) , units = c("days")))
  
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
      return (NA)
    }
    
    all.times <- xts(order.by=seq(dates[1], dates[length(ts)], by=byflag))
    
    # Merge - missing values are NA
    out <- merge(all.times, ts, all=T)
    return (out)
  }
  return (ts)
}