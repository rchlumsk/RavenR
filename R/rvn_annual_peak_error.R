#' Annual Peak Errors
#'
#' rvn_annual_peak_error creates a plot of the annual observed and simulated peak
#' percent errors.
#'
#' This function creates a plot of the percent errors in simulated peaks for
#' each water year. The peaks are calculated as the magnitude of the largest
#' event in each water year. Note that the rvn_annual_peak_error function is first
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
#' The add_labels will add the labels of 'overprediction' and 'underprediction'
#' to the right hand side axis if set to TRUE. This is useful in interpreting
#' the plots.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_labels optionally adds labels for overpredict/underpredict on
#' right side axis (default TRUE)
#' @return \item{df_peak_error}{data frame of the calculated peak errors}
#' @seealso \code{\link{annual_peak_event}} to consider the timing of peak
#' events \code{\link{annual_peak_event_error}} to calculate errors in peak
#' events
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual peak error diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph_data)
#' sim <- hydrograph_data$hyd$Sub36
#' obs <- hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of peak annual errors with default options
#' rvn_annual_peak_error(sim,obs,rplot=T,add_line=T)
#'
#' # do not plot, just store the calculated peak errors
#' peak_errors <- rvn_annual_peak_error(sim,obs,rplot=F)
#' peak_errors
#'
#' @export rvn_annual_peak_error
rvn_annual_peak_error <- function (sim, obs, rplot = T, add_line = T, add_labels = T){
  df.peak <- annual.peak(sim, obs, rplot = F)
  errs <- (df.peak$sim.peak - df.peak$obs.peak)/df.peak$obs.peak *
    100
  text.labels <- year(df.peak$date.end)
  if (rplot) {
    x.lab <- "Date (Water Year Ending)"
    y.lab <- "% Error in Peaks"
    title.lab <- ""
    if (add_line) {
      y.max <- max(0.5, max(errs))
      y.min <- min(-0.5, min(errs))
    }
    else {
      y.max <- max(errs)
      y.min <- min(errs)
    }

    df.plot <- data.frame(cbind(text.labels,errs))
    df.plot$text.labels <- as.factor(df.plot$text.labels)

    p1 <- ggplot(data=df.plot)+
      geom_point(aes(x=text.labels,y=errs))+
      scale_y_continuous(limits=c(y.min,y.max),name=y.lab)+
      scale_x_discrete(name=x.lab)+
      theme_bw()
    if (add_line) {
      p1 <- p1+
        geom_hline(yintercept=0,linetype=2)
    }

    if (add_labels) {
      if (max(errs, na.rm = T)/2 > 0) {
        p1 <- p1+
          annotate("text",x=max(as.numeric(df.plot$text.labels)+0.5),y=max(errs,na.rm=T)/2,label="Overpredict",angle=90)
      }
      if (min(errs, na.rm = T)/2 < 0) {
        p1 <- p1+
          annotate("text",x=max(as.numeric(df.plot$text.labels)+0.5),y=min(errs,na.rm=T)/2,label="Underpredict",angle=90)
      }
    }
    df <- data.frame(date.end = df.peak$date.end, errors = errs)
    return(list(df.peak.error = df,plot=p1))
  } else {
    df <- data.frame(date.end = df.peak$date.end, errors = errs)
    return(df.peak.error=df)
  }
}
