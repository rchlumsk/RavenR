#' Annual Peak Comparison
#'
#' rvn_annual_peak creates a plot of the annual observed and simulated peaks, based on the water year.
#'
#' This function creates a scatterplot of the annual observed and simulated
#' peaks, calculated for each available water year of data (Oct 1st hardcoded)
#' within the two series provided. Note that the calculation uses the peak
#' magnitude of simulated and observed series in each water year, without
#' considering the timing of the events in each series.
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
#' @param add_line optionally adds a 1:1 line to the plot for reference (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param rplot boolean whether to print the plot (default FALSE)
#' @return returns a list with peak data in a data frame, and a ggplot object
#'  \item{df_peak}{data frame of the calculated peaks}
#'  \item{p1}{ggplot object with plotted annual peaks}
#'
#' @seealso \code{\link{rvn_annual_volume}} to create a scatterplot of annual flow
#' volumes \code{\link{rvn_annual_peak_event}} to consider the timing of peak
#' events.
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
#' # create a plot of annual peaks with default options
#' peak1 <- rvn_annual_peak(sim, obs, add_line=T, add_r2=F)
#' peak1$df_peak
#' peak1$p1
#'
#' # plot with r2 and axes to zero; store results
#' peak_df <- rvn_annual_peak(sim, obs, add_line=T,
#' add_r2=T, rplot=T)
#'
#' @keywords Raven annual peak diagnostics
#' @export rvn_annual_peak
rvn_annual_peak <- function(sim, obs, add_line = T,
                             add_r2 = F, add_eqn = F, rplot = F) {
  max.sim <- rvn_apply_wyearly(sim, RavenR::rvn_which_max_xts)
  max.obs <- rvn_apply_wyearly(obs, RavenR::rvn_which_max_xts)
  dates <- max.sim[, 1]
  max.sim <- max.sim[, 2]
  max.obs <- max.obs[, 2]
  df <- data.frame(date.end = dates, sim.peak = max.sim, obs.peak = max.obs)

  if (add_r2) {
    max.obs.mean <- mean(max.obs)
    ss.err <- sum((max.sim - max.obs)^2)
    ss.tot <- sum((max.obs - max.obs.mean)^2)
    text.labels <- year(dates)
    r2 <- 1 - ss.err/ss.tot
  }

  x.lab <- expression("Observed Peak Discharge ["*m^3*"/s]")
  y.lab <- expression("Simulated Peak Discharge ["*m^3*"/s]")
  x.lim = c(min(max.obs, max.sim, na.rm = T) * 0.9,
            max(max.obs, max.sim, na.rm = T) * 1.1)
  y.lim = c(min(max.obs, max.sim, na.rm = T) * 0.9,
            max(max.obs, max.sim, na.rm = T) * 1.1)

  #text.labels <- year(dates)

  p1 <- ggplot(data=df,aes(x=max.obs,y=max.sim))+
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
      geom_text(x= max(max.obs, max.sim),
                y= min(max.sim, max.obs),
                vjust = 1,
                label=r2.label,
                parse=T,
                size = 3.5)
  }

  if (add_eqn){
    m <- lm(max.sim ~ 0 + max.obs)
    coeff <- round(as.numeric(m$coefficients[1]), 3)
    equation <- paste0( "y = ", coeff, "x")
    p1 <- p1 +
      geom_text(x = max(max.obs, max.sim),
                y = min(max.sim, max.obs)*1.1,
                vjust = 1,
                label = equation,
                size = 3.5)
  }

  if (rplot) {plot(p1)}

  return(list(df_peak = df,p1=p1))
}

