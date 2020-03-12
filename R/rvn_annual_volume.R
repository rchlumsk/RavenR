#' Annual Volume Comparison
#'
#' rvn_annual_volume creates a plot of the annual observed and simulated volumes.
#'
#' This function creates a scatterplot of the annual observed and simulated
#' volumes, calculated for each available water year of data (Oct 1st
#' hardcoded) within the two series provided. The sim and obs should be of time
#' series (xts) format and are assumed to be of the same length and time
#' period. Note that missing values in the observed series will impact the
#' volume estimation, and it is recommended that the NA values are filled in
#' prior to use of this function.
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
#' @return \item{df_volume}{data frame of the calculated annual volumes}
#' @seealso \code{\link{flow_scatterplot}} to create a scatterplot of flow
#' values
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual volume diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph_data)
#' sim <- hydrograph_data$hyd$Sub36
#' obs <- hydrograph_data$hyd$Sub36_o
#'
#' # create a plot of the annual volumes with defaults
#' rvn_annual_volume(sim,obs)
#'
#' # create a plot of the annual volumes with r2 and axis set to zero
#' rvn_annual_volume(sim,obs,add_r2=T,axis_zero=T)
#'
#' # store results of annual volumes
#' volumes <- rvn_annual_volume(sim,obs,rplot=F)
#'
#' @export rvn_annual_volume
rvn_annual_volume <- function(sim,obs,rplot=T,add_line=T,add_r2=F,axis_zero=F) {

  sec.per.day <- 86400

  #calculate the sums
  sum.sim <- apply_wyearly(sim, sum,na.rm=T)
  dates <- sum.sim[,1]
  sum.sim <- sum.sim[,2]
  sum.obs <- apply_wyearly(obs, sum,na.rm=T)[,2]

  # unit conversion
  sum.sim <- sum.sim*sec.per.day
  sum.obs <- sum.obs*sec.per.day

  # calculate the r2 fit
  if (add_r2) {
    # need to check the r2 calculation, ensure it is for an intercept of zero
    sum.obs.mean <- mean(sum.obs)
    ss.err <- sum((sum.sim - sum.obs)^2)
    ss.tot <- sum((sum.obs - sum.obs.mean)^2)
    r2 <- 1- ss.err/ss.tot
  }

  if (rplot) {
    x.lab <- "Observed Volume [m3]"
    y.lab <- "Simulated Volume [m3]"
    title.lab <- '' # "Annual Volume Comparison"
    if (axis_zero) {
      x.lim=c(0,max(sum.obs,sum.sim,na.rm=T)*1.1)
      y.lim=c(0,max(sum.obs,sum.sim,na.rm=T)*1.1)
    } else {
      x.lim=c(min(sum.obs,sum.sim,na.rm=T)*0.9,max(sum.obs,sum.sim,na.rm=T)*1.1)
      y.lim=c(min(sum.obs,sum.sim,na.rm=T)*0.9,max(sum.obs,sum.sim,na.rm=T)*1.1)
    }

    text.labels <- year(dates)
    plot(coredata(sum.obs), coredata(sum.sim), xlim=x.lim, ylim=y.lim, xlab=x.lab, ylab=y.lab, main=title.lab)
    text(coredata(sum.obs), coredata(sum.sim), text.labels, cex=0.75, pos=3)
    if (add_line) { abline(0,1,lty=2) }
    if (add_r2) {  mtext(sprintf('R2 = %.2f',r2), side=3,adj=1) }
  }
  df <- data.frame("date_end"=dates,"sim_vol"=sum.sim,"obs_vol"=sum.obs)
  return("df_volume"=df)
}
