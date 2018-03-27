#' Annual Peak Event Comparison
#'
#' annual.peak.event creates a plot of the annual observed and simulated peaks.
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
#' @param add.line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add.r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param axis.zero optionally sets the minimum volume on axes to zero (default
#' FALSE)
#' @return \item{df.peak.event}{data frame of the calculated peak events}
#' @seealso \code{\link{annual.peak}} to create a scatterplot of annual peaks
#' (consider the magnitude of peaks only)
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual peak event diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # create a plot of annual peak events with default options
#' annual.peak.event(sim,obs,rplot=T,add.line=T,add.r2=F,axis.zero=F)
#'
#' # plot with r2 and axes to zero; store results
#' peak.df <- annual.peak.event(sim,obs,rplot=T,add.line=T,add.r2=T,axis.zero=T)
#' peak.df
#'
#' # store results without plotting
#' peak.df <- annual.peak.event(sim,obs,rplot=F)
#' peak.df
#'
#' @export annual.peak.event
annual.peak.event <- function(sim,obs,rplot=T,add.line=T,add.r2=F,axis.zero=F) {

  # calculate the maximum observed in each year
  max.obs <- apply.wyearly(obs, max, na.rm=T)[,2]
  max.dates <- as.Date(apply.wyearly(obs, function(x) toString(lubridate::date(which.max.xts(x))))[,2])

  ind <- matrix(NA,nrow=length(max.obs),ncol=1)
  for (k in 1:length(max.obs)) {
    ind[k] <- which(year(obs)==year(max.dates[k]) & month(obs)==month(max.dates[k]) & day(obs)==day(max.dates[k]))
  }
  # get the simulated values at those indices
  max.sim <- as.numeric(sim[ind])

  # calculate the r2 fit
  if (add.r2) {
    # need to check the r2 calculation, ensure it is for an intercept of zero
    max.obs.mean <- mean(max.obs)
    ss.err <- sum((max.sim - max.obs)^2)
    ss.tot <- sum((max.obs - max.obs.mean)^2)
    r2 <- 1- ss.err/ss.tot
  }

  if (rplot) {

    x.lab <- "Observed Peak Event [m3/s]"
    y.lab <- "Simulated Peak Event [m3/s]"
    title.lab <- '' #"Annual Peak Event Flow Comparison"
    if (axis.zero) {
      x.lim=c(0,max(max.obs,max.sim,na.rm=T)*1.1)
      y.lim=c(0,max(max.obs,max.sim,na.rm=T)*1.1)
    } else {
      x.lim=c(min(max.obs,max.sim,na.rm=T)*0.9,max(max.obs,max.sim,na.rm=T)*1.1)
      y.lim=c(min(max.obs,max.sim,na.rm=T)*0.9,max(max.obs,max.sim,na.rm=T)*1.1)
    }
    text.labels <- sprintf("'%02d",as.numeric(format(index(max.sim), format = "%Y")) )
    plot(coredata(max.obs), coredata(max.sim), xlim=x.lim, ylim=y.lim, xlab=x.lab, ylab=y.lab, main=title.lab)
    text(coredata(max.obs), coredata(max.sim), text.labels, cex=0.75, pos=3)
    if (add.line) { abline(0,1,lty=2) }
    if (add.r2) {  mtext(sprintf('R2 = %.2f',r2), side=3,adj=1) }
  }
  df <- data.frame("obs.dates"=max.dates,"sim.peak.event"=max.sim,"obs.peak.event"=max.obs)
  return("df.peak.event"=df)
}
