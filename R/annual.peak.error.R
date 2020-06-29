#' Annual Peak Errors
#'
#' annual.peak.error creates a plot of the annual observed and simulated peak
#' percent errors.
#'
#' This function creates a plot of the percent errors in simulated peaks for
#' each water year. The peaks are calculated as the magnitude of the largest
#' event in each water year. Note that the annual.peak.error function is first
#' used to obtain the peaks in each year, then the percent errors are
#' calculated.
#'
#' The percent errors are calculated as (QPsim-QPobs)/QPobs*100, where QP is
#' the peak flow event.
#'
#' The sim and obs should be of time series (xts) format and are assumed to be
#' of the same length and time period. The flow series are assumed to be daily
#' flows with units of m3/s.
#'
#' The R2 diagnostic is calculated for a fit with no intercept (in a perfect
#' fit the points are identical, and intercept is automatically zero).
#'
#' The add.labels will add the labels of 'overprediction' and 'underprediction'
#' to the right hand side axis if set to TRUE. This is useful in interpreting
#' the plots.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add.line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add.labels optionally adds labels for overpredict/underpredict on
#' right side axis (default TRUE)
#' @return \item{df.peak.error}{data frame of the calculated peak errors}
#' @seealso \code{\link{annual.peak.event}} to consider the timing of peak
#' events \code{\link{annual.peak.event.error}} to calculate errors in peak
#' events
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual peak error diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # create a plot of peak annual errors with default options
#' annual.peak.error(sim,obs,rplot=T,add.line=T)
#'
#' # do not plot, just store the calculated peak errors
#' peak.errors <- annual.peak.error(sim,obs,rplot=F)
#' peak.errors
#'
#' @export annual.peak.error
annual.peak.error <- function(sim,obs,rplot=T,add.line=T,add.labels=T) {

  # obtain peak from annual.peak function
  df.peak <- annual.peak(sim,obs,rplot=F)

  # calculate the errors
  errs <- (df.peak$sim.peak - df.peak$obs.peak)/df.peak$obs.peak*100
  text.labels <- year(df.peak$date.end)

  if (rplot) {
    x.lab <- "Date (Water Year Ending)"
    y.lab <- "% Error in Peaks"
    title.lab <- ''
    if (add.line) {
      y.max <- max(0.5,max(errs))
      y.min <- min(-0.5,min(errs))
    } else {
      y.max <- max(errs)
      y.min <- min(errs)
    }
    plot(errs, xlab=x.lab, ylab=y.lab, main=title.lab,xaxt='n',ylim=c(y.min,y.max))
    axis(1, at=index(errs),labels=text.labels)
    if (add.line) { abline(h=0,lty=2) }
    if (add.labels) {
      if (max(errs,na.rm=T)/2 > 0 ) {
        mtext('overpredict',side=4,at=c(max(errs,na.rm=T)/2),cex=0.8)
      }
      if (min(errs,na.rm=T)/2 < 0 ) {
        mtext('underpredict',side=4,at=c(min(errs,na.rm=T)/2),cex=0.8)
      }
    }
  }
  df <- data.frame("date.end"=df.peak$date.end,"errors"=errs)
  return("df.peak.error"=df)
}
