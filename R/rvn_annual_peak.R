#' Annual Peak Comparison
#'
#' rvn_annual_peak creates a plot of the annual observed and simulated peaks.
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
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param axis_zero optionally sets the minimum volume on axes to zero (default
#' FALSE)
#' @return \item{df_peak}{data frame of the calculated peaks}
#' @seealso \code{\link{annual_volume}} to create a scatterplot of annual flow
#' volumes \code{\link{rvn_annual_peak_event}} to consider the timing of peak
#' events
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual peak diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph_data)
#' sim <- hydrograph_data$hyd$Sub36
#' obs <- hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of annual peaks with default options
#' rvn_annual_peak(sim,obs,rplot=T,add_line=T,add_r2=F,axis_zero=F)
#'
#' # plot with r2 and axes to zero; store results
#' peak_df <- rvn_annual_peak(sim,obs,rplot=T,add_line=T,add_r2=T,axis_zero=T)
#'
#' # store results without plotting
#' peak_df <- rvn_annual_peak(sim,obs,rplot=F)
#'
#' @export rvn_annual_peak
rvn_annual_peak <- function(sim,obs,rplot=T,add_line=T,add_r2=F,axis_zero=F) {

  # calculate the maximums
  max.sim <- rvn_apply_wyearly(sim, RavenR::which_max_xts)
  max.obs <- rvn_apply_wyearly(obs, RavenR::which_max_xts)
  dates <- max.sim[,1]
  max.sim <- max.sim[,2]
  max.obs <- max.obs[,2]

  # calculate the r2 fit
  if (add_r2) {
    # need to check the r2 calculation, ensure it is for an intercept of zero
    max.obs.mean <- mean(max.obs)
    ss.err <- sum((max.sim - max.obs)^2)
    ss.tot <- sum((max.obs - max.obs.mean)^2)
    text.labels <- year(dates)
    r2 <- 1- ss.err/ss.tot
  }

  if (rplot) {

    x.lab <- "Observed Peak [m3/s]"
    y.lab <- "Simulated Peak [m3/s]"
    title.lab <- '' #"Annual Peak Flow Comparison"
    if (axis_zero) {
      x.lim=c(0,max(max.obs,max.sim,na.rm=T)*1.1)
      y.lim=c(0,max(max.obs,max.sim,na.rm=T)*1.1)
    } else {
      x.lim=c(min(max.obs,max.sim,na.rm=T)*0.9,max(max.obs,max.sim,na.rm=T)*1.1)
      y.lim=c(min(max.obs,max.sim,na.rm=T)*0.9,max(max.obs,max.sim,na.rm=T)*1.1)
    }
    text.labels <- sprintf("'%02d",as.numeric(format(index(max.sim), format = "%Y")) )
    plot(coredata(max.obs), coredata(max.sim), xlim=x.lim, ylim=y.lim, xlab=x.lab, ylab=y.lab, main=title.lab)
    text(coredata(max.obs), coredata(max.sim), text.labels, cex=0.75, pos=3)
    if (add_line) { abline(0,1,lty=2) }
    if (add_r2) {  mtext(sprintf('R2 = %.2f',r2), side=3,adj=1) }
  }
  df <- data.frame("date_end"=dates,"sim_peak"=max.sim,"obs_peak"=max.obs)
  return("df_peak"=df)
}
