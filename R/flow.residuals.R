#' Scatterplot of model flows
#'
#' flow.residuals creates a residuals time series for flow values. Useful in
#' diagnotic analysis of model outputs.
#'
#' This function creates a residuals time series plot for flow values, with the
#' option to smooth out the values using the rollmean function in zoo. The
#' winter months are optionally shaded in the time series; winter period is
#' defined as December 1st to March 31st.
#'
#' The residuals are calculated as sim - obs.
#'
#' The sim and obs should be of time series (xts) format. The flow series are
#' assumed to be daily flows with units of m3/s.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param ma.smooth optional length of rolling average to smooth residuals with
#' (default 3)
#' @param add.line optionally adds a horizontal line to the plot for reference
#' (default TRUE)
#' @param winter.shading optionally adds a light blue shading to winter months
#' (default TRUE)
#' @return \item{resids}{residual time series}
#' @seealso \code{\link{flow.scatterplot}} to create a scatterplot of flow
#' values
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow residual diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_ob
#'
#' # default with moving average smoothing shading of winter months
#' flow.residuals(sim,obs)
#'
#' # plot with more smoothing than the default 3
#' flow.residuals(sim,obs,ma.smooth=10)
#'
#' # turn off the smoothing and winter shading
#' flow.residuals(sim,obs,ma.smooth = 0,winter.shading = F)
#'
#' @export flow.residuals
flow.residuals <- function(sim,obs,ma.smooth=3,add.line=T,winter.shading=T) {

  if ( ma.smooth < 0) {
    stop("Requires a non-negative integer for ma.smooth ")
  }

  if  (ma.smooth == 0 ) {
    resids <- sim-obs
  } else {
    resids <- rollapply(sim-obs,ma.smooth,mean,fill=NA)
  }

  if (winter.shading) {
    plot(lubridate::date(resids),resids,xlab='Date',ylab='Smoothed Residuals',
         type='l',col='white',ylim=c(min(resids,na.rm=T),max(c(resids),na.rm=T)))
    # shaded winter months
    x <- resids
    temp <- x[((month(x[,1]) == 12) & (day(x[,1]) == 1)) | ((month(x[,1]) == 3) & (day(x[,1]) == 31))]
    ep <- match(lubridate::date(temp),lubridate::date(x))
    if (month(sim[ep[1]])==3) {
      ep <- ep[-1]
    }
    if (month(sim[ep[length(ep)]])==12) {
      ep <- ep[-length(ep)]
    }
    bc <- col.transparent('cyan',50)
    for (k in seq(1,length(ep),2)) {
      cord.x <- c(lubridate::date(x[ep[k]]),lubridate::date(x[ep[k]]),lubridate::date(x[ep[k+1]]),lubridate::date(x[ep[k+1]]))
      cord.y <- c(min(resids,na.rm=T),max(resids,na.rm=T),max(resids,na.rm=T),min(resids,na.rm=T))
      polygon(cord.x,cord.y,col=bc,border=NA)
    }
    lines(lubridate::date(resids),resids,col='black')
  } else {
    plot(lubridate::date(resids),resids,xlab='Date',ylab='Smoothed Residuals',
         type='l',col='black',ylim=c(min(resids,na.rm=T),max(c(resids),na.rm=T)))
  }
  if (add.line) {abline(h=0,col='blue',lty=5) }

  return(resids)
}

