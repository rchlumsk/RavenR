#' Annual Peak Event Errors
#'
#' annual.peak.event.error creates a plot of the annual observed and simulated
#' peak event errors.
#'
#' This function creates a plot of the percent errors in simulated peak events
#' for each water year. The peaks are calculated as using flows from the same
#' day as the peak event in the observed series, i.e. the timing of the peak is
#' considered here. Note that the annual.peak.event function is first used to
#' obtain the peaks in each year, then the percent errors are calculated.
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
#' @return \item{df.peak.event.error}{data frame of the calculated peak event
#' errors}
#' @seealso \code{\link{annual.peak}} to consider just the magnitude of each
#' year's peak \code{\link{annual.peak.error}} to calculate errors in peaks
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual peak event error diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # create a plot of peak annual errors with default options
#' annual.peak.event.error(sim,obs,rplot=T,add.line=T)
#'
#' # do not plot, just store the calculated peak errors
#' peak.event.errors <- annual.peak.event.error(sim,obs,rplot=F)
#'
#' @export annual.peak.event.error
annual.peak.event.error <- function(sim,obs,rplot=T,add.line=T,add.labels=T) {

  # obtain peak from annual.peak.event function
  df.peak.event <- annual.peak.event(sim,obs,rplot=F)

  # calculate the errors
  errs <- (df.peak.event$sim.peak.event - df.peak.event$obs.peak.event)/df.peak.event$obs.peak.event*100
  text.labels <- year(df.peak.event$obs.dates)

  if (rplot) {
    x.lab <- "Date (Water Year Ending)"
    y.lab <- "% Error in Event Peaks"
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
  df <- data.frame("obs.dates"=df.peak.event$obs.dates,"errors"=errs)
  return("df.peak.event.error"=df)
}
