#' Annual Peak Event Comparison
#'
#' rvn_annual_peak_event creates a plot of the annual observed and simulated peaks,
#' based on the water year.
#'
#' This function creates a scatterplot of the annual observed and simulated
#' peaks, calculated for each available water year of data (Oct 1st hardcoded)
#' within the two series provided; note that the difference between this and
#' the annual.peak function is that here the peak event simulated for the same
#' day as the peak event in observed data is used, instead of the largest
#' recorded simulated event. In some sense this captures the timing of the
#' event, i.e. the peak event must be simulated on the same day as the observed
#' peak to be captured well.
#'
#' The sim and obs should be of time series (xts) format and are assumed to be
#' of the same length and time period. The flow series are assumed to be daily
#' flows with units of m3/s.
#'
#' The R2 diagnostic is calculated for a fit with no intercept (in a perfect
#' fit the points are identical, and intercept is automatically zero).
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param rplot boolean whether to print the plot (default FALSE)
#' @return returns a list with peak data in a data frame, and a ggplot object
#'  \item{df_peak_event}{data frame of the calculated peak events}
#'  \item{p1}{ggplot object with plotted annual peaks}
#'
#' @seealso \code{\link{rvn_annual_peak}} to create a scatterplot of annual peaks
#' (consider the magnitude of peaks only)
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#'
#' @examples
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of annual peak events with default options
#' peak1 <- rvn_annual_peak_event(sim, obs)
#' peak1$df_peak_event
#' peak1$p1
#'
#' # add the r2 regression line and plot directly
#' rvn_annual_peak_event(sim, obs, add_r2=T, rplot=T)
#'
#'
#' @keywords Raven annual peak event diagnostics
#' @export rvn_annual_peak_event
rvn_annual_peak_event <- function (sim, obs, add_line = T, add_r2 = F, rplot = F) {
  max.obs <- rvn_apply_wyearly(obs, max, na.rm = T)[, 2]
  max.dates <- as.Date(rvn_apply_wyearly(obs, function(x) toString(lubridate::date(rvn_which_max_xts(x))))[,
                                                                                                   2])
  ind <- matrix(NA, nrow = length(max.obs), ncol = 1)
  for (k in 1:length(max.obs)) {
    ind[k] <- which(year(obs) == year(max.dates[k]) & month(obs) ==
                      month(max.dates[k]) & day(obs) == day(max.dates[k]))
  }
  max.sim <- as.numeric(sim[ind])

  df <- data.frame(obs.dates = max.dates, sim.peak.event = max.sim,
                   obs.peak.event = max.obs)

  if (add_r2) {
    max.obs.mean <- mean(max.obs)
    ss.err <- sum((max.sim - max.obs)^2)
    ss.tot <- sum((max.obs - max.obs.mean)^2)
    r2 <- 1 - ss.err/ss.tot
  }

  x.lab <- expression("Observed Peak Discharge ("*m^3*"/s)")
  y.lab <- expression("Simulated Peak Discharge ("*m^3*"/s)")
  #title.lab <- ""

  x.lim = c(min(max.obs, max.sim, na.rm = T) * 0.9,
            max(max.obs, max.sim, na.rm = T) * 1.1)
  y.lim = c(min(max.obs, max.sim, na.rm = T) * 0.9,
            max(max.obs, max.sim, na.rm = T) * 1.1)

  #text.labels <- year(max.dates)

  p1 <- ggplot(data=df,aes(x=obs.peak.event,y=sim.peak.event))+
    geom_point()+
    scale_x_continuous(limits=x.lim, name=x.lab)+
    scale_y_continuous(limits=y.lim, name=y.lab)+
    theme_RavenR()

  if (add_line){
    p1 <- p1 +
      geom_abline(linetype=2)
  }

  if (add_r2){
    r2.label <- paste("R^2 == ", round(r2,2))
    p1 <- p1 +
      geom_text(x= x.lim[2],
                y= y.lim[1]*1.9,
                vjust = 1,
                hjust = 1,
                label=r2.label,
                parse=T,
                size = 3.5)
  }

  if (rplot) {plot(p1)}

  return(list(df_peak_event = df,p1=p1))
}
