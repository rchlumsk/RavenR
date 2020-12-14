#' @title Annual Peak Errors
#'
#' @description
#' rvn_annual_peak_error creates a plot of the annual observed and simulated peak
#' percent errors, based on the water year.
#'
#' @details
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
#' The add_labels will add the labels of 'overprediction' and 'underprediction'
#' to the right hand side axis if set to TRUE. This is useful in interpreting
#' the plots.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param mm month of water year (default 9)
#' @param dd day of water year (default 30)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_labels optionally adds labels for overpredict/underpredict on
#' right side axis (default TRUE)
#' @return returns a list with peak errors in a data frame, and a ggplot object
#'  \item{df_peak_error}{data frame of the calculated peak errors}
#'  \item{p1}{ggplot object with plotted annual peak errors}
#'
#' @seealso \code{\link{rvn_annual_peak_event}} to consider the timing of peak
#' events \code{\link{rvn_annual_peak_event_error}} to calculate errors in peak
#' events.
#'
#' @examples
#'
#' system.file("extdata","run1_Hydrographs.csv", package="RavenR") %>%
#' rvn_hyd_read(.) %>%
#' rvn_hyd_extract(subs="Sub36",.) ->
#' hyd_data
#'
#' sim <- hyd_data$sim
#' obs <- hyd_data$obs
#'
#' # create a plot of annual peak errors with default options
#' peak1 <- rvn_annual_peak_error(sim, obs)
#' peak1$df_peak_error
#' peak1$p1
#'
#' # plot directly and without labels
#' rvn_annual_peak_error(sim, obs, add_line=TRUE, add_labels=FALSE)
#'
#' @export rvn_annual_peak_error
#' @importFrom stats lm
#' @importFrom lubridate year date
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_text scale_x_discrete scale_y_continuous
rvn_annual_peak_error <- function(sim, obs, mm=9, dd=30, add_line = TRUE, add_labels = TRUE)
{

  df.peak <- rvn_annual_peak(sim, obs, mm=mm, dd=dd)$df_peak
  errs <- (df.peak$sim.peak - df.peak$obs.peak)/df.peak$obs.peak *
    100
  text.labels <- lubridate::year(df.peak$date.end)

  x.lab <- "Date (Water Year Ending)"
  y.lab <- "% Error in Peaks"
  #title.lab <- ""
  if (add_line) {
    limit <- max(max(errs), abs(min(errs)))
    y.max <- max(0.5, limit)
    y.min <- min(-0.5, limit *-1)
  } else {
    y.max <- limit
    y.min <- limit*-1
  }

  df.plot <- data.frame(cbind(text.labels,errs))
  df.plot$text.labels <- as.factor(df.plot$text.labels)

  p1 <- ggplot(data=df.plot)+
    geom_point(aes(x=text.labels,y=errs))+
    scale_y_continuous(limits=c(y.min,y.max),name=y.lab)+
    scale_x_discrete(name=x.lab)+
    rvn_theme_RavenR()
  if (add_line) {
    p1 <- p1+
      geom_hline(yintercept=0,linetype=2)
  }

  if (add_labels) {
    p1 <- p1+
      geom_text(x= max(as.numeric(df.plot$text.labels)+0.5),
                y= y.max/2,
                label= "Overpredict",
                angle=90,
                vjust = 0.5,
                hjust = 0.5)
    p1 <- p1+
      geom_text(x=max(as.numeric(df.plot$text.labels)+0.5),
                y= y.min/2,
                label="Underpredict",
                angle=90,
                vjust = 0.5,
                hjust = 0.5)
  }
  df <- data.frame(date.end = df.peak$date.end, errors = errs)

  return(list(df_peak_error = df,p1=p1))
}
