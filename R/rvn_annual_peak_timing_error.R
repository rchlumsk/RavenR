#' @title Annual Peak Timing Errors
#'
#' @description
#' rvn_annual_peak_timing_error creates a plot of the annual observed and simulated
#' peak timing errors, based on the water year.
#'
#' @details
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
#' The add_labels will add the labels of 'early peak' and 'late peak' to the
#' right hand side axis if set to TRUE. This is useful in interpreting the
#' plots. Note that values in this metric of less than zero indicate an early
#' prediction of the peak, and positive values mean a late prediction of the
#' peak (since the values are calculated as day index of simulated peak - day
#' index of observed peak).
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param mm month of water year (default 9)
#' @param dd day of water year (default 30)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_labels optionally adds labels for early peak/late peaks on right
#' side axis (default TRUE)
#'
#' @return returns a list with peak timing errors in a data frame, and a ggplot object
#'  \item{df_peak_timing_error}{data frame of the calculated peak timing errors}
#'  \item{p1}{ggplot object with plotted annual peak errors}
#'
#' @seealso \code{\link{rvn_annual_peak_event}} to consider the timing of peak
#' events \code{\link{rvn_annual_peak_event_error}} to calculate errors in peak
#' events
#'
#' @examples
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of peak timing errors with defaults
#' peak1 <- rvn_annual_peak_timing_error(sim, obs, add_line=TRUE)
#' peak1$df_peak_timing_error
#' peak1$p1
#'
#' # plot directly and without labels
#' rvn_annual_peak_timing_error(sim, obs, add_line=TRUE, add_labels=FALSE)
#'
#'
#' @export rvn_annual_peak_timing_error
#' @importFrom stats lm
#' @importFrom lubridate year date
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_text scale_x_discrete scale_y_continuous
rvn_annual_peak_timing_error <- function (sim, obs, mm=9, dd=30, add_line = TRUE, add_labels = TRUE)
{

  max.obs <- rvn_apply_wyearly_which_max_xts(obs, mm=mm, dd=dd)
  max.obs.dates <- lubridate::date(max.obs)
  max.sim <- rvn_apply_wyearly_which_max_xts(sim, mm=mm, dd=dd)
  max.sim.dates <- lubridate::date(max.sim)
  date.end <- lubridate::date(sim[rvn_wyear_indices(sim, mm=mm, dd=dd)])
  errs <- as.numeric(max.sim.dates - max.obs.dates)
  text.labels <- year(date.end)

  x.lab <- "Date (Water year ending)"
  y.lab <- "Day Difference in Peaks"
  title.lab <- ""

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
                label= "Late Peak",
                angle=90,
                vjust = 0.5,
                hjust = 0.5,
                size = 3.5)
    p1 <- p1+
      geom_text(x=max(as.numeric(df.plot$text.labels)+0.5),
                y= y.min/2,
                label="Early Peak",
                angle=90,
                vjust = 0.5,
                hjust = 0.5,
                size = 3.5)

  }
  df <- data.frame(date.end = date.end, peak.timing.errors = errs)

  return(list(df_peak_timing_error = df,p1=p1))
}
