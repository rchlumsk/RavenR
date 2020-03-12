#' Annual Peak Timing Errors
#'
#' rvn_annual_peak_timing_error creates a plot of the annual observed and simulated
#' peak timing errors.
#'
#' This function creates a plot of the peak timing errors in simulated peaks
#' for each water year. The difference in days between the simulated peak and
#' observed peak are plotted (and/or returned in the data frame) for the water
#' year. This diagnostic is useful in determining how accurate the timing of
#' peak predictions is. Note that a large error in the number of days between
#' simulated and observed peaks indicates that the model predicted a larger
#' event at a different time of year, i.e. overestimated a different event or
#' underestimated the actual peak event, relative to the observed flow series.
#'
#' The sim and obs should be of time series (xts) format and are assumed to be
#' of the same length and time period. The flow series are assumed to be daily
#' flows with units of m3/s. Note that a plot title is purposely omitted in
#' order to allow the automatic generation of plot titles.
#'
#' The add.labels will add the labels of 'early peak' and 'late peak' to the
#' right hand side axis if set to TRUE. This is useful in interpreting the
#' plots. Note that values in this metric of less than zero indicate an early
#' prediction of the peak, and positive values mean a late prediction of the
#' peak (since the values are calculated as day index of simulated peak - day
#' index of observed peak).
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_labels optionally adds labels for early peak/late peaks on right
#' side axis (default TRUE)
#' @return \item{df_peak_timing_error}{data frame of the calculated peak timing
#' errors}
#' @seealso \code{\link{annual_peak_event}} to consider the timing of peak
#' events \code{\link{annual_peak_event_error}} to calculate errors in peak
#' events
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual peak timing error diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph_data)
#' sim <- hydrograph_data$hyd$Sub36
#' obs <- hydrograph_data$hyd$Sub36_o
#'
#' # create a plot of peak timing errors with defaults
#' rvn_annual_peak_timing_error(sim,obs)
#'
#' # create the plot without labels or 1:1 line or labels
#' rvn_annual_peak_timing_error(sim,obs,add_line=F,add_labels=F)
#'
#' # avoid plotting and
#' timing_errs <- rvn_annual_peak_timing_error(sim,obs,rplot=F)
#'
#' @export rvn_annual_peak_timing_error
rvn_annual_peak_timing_error <- function(sim,obs,rplot=T,add_line=T,add_labels=T) {


  # get the date of maximum obs event
  max.obs <- apply_wyearly(obs, max, na.rm=T)[,2]
  max.obs.dates <- as.Date(apply_wyearly(obs, function(x) toString(lubridate::date(which_max_xts(x))))[,2])

  # get the date of maximum sim event
  max.sim <- apply_wyearly(sim, max, na.rm=T)[,2]
  max.sim.dates <- as.Date(apply_wyearly(sim, function(x) toString(lubridate::date(which_max_xts(x))))[,2])

  # get water year end dates
  date.end <- apply_wyearly(sim,mean)[,1]

  # calcualte errors in timing
  errs <- as.numeric(max.sim.dates - max.obs.dates)
  text.labels <- year(date.end)

  if (rplot) {
    x.lab <- "Date (Water year ending)"
    y.lab <- "Day Difference in Peaks"
    title.lab <- ''
    if (add_line) {
      y.max <- max(0.5,max(errs))
      y.min <- min(-0.5,min(errs))
    } else {
      y.max <- max(errs)
      y.min <- min(errs)
    }
    plot(errs, xlab=x.lab, ylab=y.lab, main=title.lab,xaxt='n',ylim=c(y.min,y.max))
    if (add_line) { abline(h=0,lty=2) }
    axis(1, at=index(errs),labels=text.labels)

    if (add_labels) {
      if (max(errs,na.rm=T)/2 > 0 ) {
        mtext('late peak',side=4,at=c(max(errs,na.rm=T)/2),cex=0.8)
      }
      if (min(errs,na.rm=T)/2 < 0 ) {
        mtext('early peak',side=4,at=c(min(errs,na.rm=T)/2),cex=0.8)
      }
    }

  }
  df <- data.frame("date_end"=date.end,"peak_timing_errors"=errs)
  return("df_peak_timing_error"=df)
}

