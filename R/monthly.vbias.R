#' Monthly Volume Bias
#'
#' monthly.vbias creates a plot of the monthly volume biases in the simulated
#' flow series.
#'
#' This function calculates the monthly volume biases and optionally creates a
#' plot of them. The monthly volume biases are averaged across all years of
#' data. If normalized, the biases are calculated as:
#'
#' (Vi_sim - Vi_obs)/Vi_obs*100
#'
#' to be expressed as a percent error.
#'
#' The sim and obs should be of time series (xts) format and are assumed to be
#' of the same length and time period. The flow series are assumed to be daily
#' flows with units of m3/s. Note that a plot title is purposely omitted in
#' order to allow the automatic generation of plot titles.
#'
#' The add.labels will add the labels of 'overestimated' and 'underestimated'
#' to the right hand side axis if set to TRUE. This is useful in interpreting
#' the plots. Note that the biases are calculated as sim_Volume - obs_Volume,
#' which means that negative values mean the volume is underestimated, and
#' positive values mean the volume is overestimated.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add.line optionally adds a horizontal line to the plot for reference
#' (default TRUE)
#' @param normalize option to normalize the biases and report as percent error
#' (default TRUE)
#' @param add.labels optionally adds labels for early peak/late peaks on right
#' side axis (default TRUE)
#' @return \item{mvbias}{monthly volume biases}
#' @seealso \code{\link{annual.volume}} to create a scatterplot of annual flow
#' volumes
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven monthly volume bias diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # check the monthly volume bias; normalizes by default
#' monthly.vbias(sim,obs)
#'
#' # check unnormalzied monthly volume biases; see the larger volumes in certain periods
#' monthly.vbias(sim,obs,normalize = F)
#'
#' @export monthly.vbias
monthly.vbias <- function(sim,obs,rplot=T,add.line=T,normalize=T,add.labels=T) {

  obs.monthly <- apply.monthly(obs,sum,na.rm=T)
  sim.monthly <- apply.monthly(sim,sum,na.rm=T)
  mvbias <- matrix(NA,nrow=12,ncol=1)
  colnames(mvbias) <-c ("mvbias")
  rownames(mvbias) <- RavenR::mos.names(T)

  # important part - calculate monthly differences, relative or not
  if (normalize) {
    diff <- (sim.monthly - obs.monthly)/obs.monthly*100
    ylabel <- '% Flow Volume Bias'
  } else {
    diff <- (sim.monthly - obs.monthly)
    ylabel <- 'Flow Volume Bias [m3/s]'
  }

  # calculate mvbias (normalized or unnormalized handled the same)
  for ( k in 1:12) {
    mvbias[k] <- mean(diff[month(diff) == k,],na.rm=T)
  }

  # create plot
  if (rplot) {
    # thick line plot, maybe upgrade to bar plot eventually
    plot(mvbias,type='h',main='',xlab='Month',xaxt='n',ylab=ylabel,
         lwd=5,panel.first=grid())
    axis(1, at=1:12,labels=RavenR::mos.names(T),cex.axis=0.6)
    if (add.line) { abline(h=0,lty=5) }
    if (add.labels) {
      if (max(mvbias,na.rm=T)/2 > 0 ) {
        mtext('overestimated',side=4,at=c(max(mvbias,na.rm=T)/2),cex=0.8)
      }
      if (min(mvbias,na.rm=T)/2 < 0 ) {
        mtext('underestimated',side=4,at=c(min(mvbias,na.rm=T)/2),cex=0.8)
      }
    }
  }
  return(mvbias)
}
