#' Cumulative Plot of model flows
#'
#' cum.plot.flow creates a cumulative flow plot of the simulated flows;
#' optionally includes an observed and/or inflow series as well. Useful in
#' diagnotic analysis of model outputs.
#'
#' This function will plot the simulated series in all cases, and will include
#' the observed and inflow plots if they are supplied.
#'
#' The sim and obs should be of time series (xts) format. The flow series are
#' assumed to be daily flows with units of m3/s.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' Note that the cumsum function does not have an na.rm=T argument, thus if
#' there are any NA values in the water year of data for any provided series,
#' the values beyond an NA value will be calculated as NA. It is up to the user
#' to handle NA values appropriately fill in or replace NA values based on the
#' type of data supplied. For flow series, linear interpolation for small periods
#' of missing values may be appropriate.
#'
#' @param sim time series object of simulated flows
#' @param obs optionally supply an inflow series to plot as well
#' @param inflow optionally supply an inflow series to plot as well
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{flow.scatterplot}} for creating flow scatterplots
#@seealso \code{\link{cum.plot}} for creating generic cumulative function plotting
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow cumulative diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # plot cumulative flow for sim and obs
#' cum.plot.flow(sim,obs)
#'
#' # plot cumulative flows for specific period
#' prd <- "2003-10-01/2004-10-01"
#' cum.plot.flow(sim[prd],obs[prd])
#'
#' @export cum.plot.flow
cum.plot.flow <- function(sim=NULL,obs=NULL,inflow=NULL) {

  sec.per.day <- 86400
  cum.sim <- NULL ; cum.obs <- NULL ; cum.inflow <- NULL

  # get the indicies of water years
  ep <- wyear.indices(sim)

  cum.sim <- matrix(NA,nrow=nrow(sim))
  for (k in 1:(length(ep)-1)) {
    cum.sim[(ep[k]):(ep[k+1])] <- cumsum(sim[(ep[k]):(ep[k+1])])
  }
  cum.sim <- cum.sim*sec.per.day

  if (!(is.null(obs))) {
    cum.obs <- matrix(NA,nrow=nrow(obs))
    for (k in 1:(length(ep)-1)) {
      cum.obs[(ep[k]):(ep[k+1])] <- cumsum(obs[(ep[k]):(ep[k+1])])
    }
    cum.obs <- cum.obs*sec.per.day
  }

  if (!(is.null(inflow))) {
    cum.inflow <- matrix(NA,nrow=nrow(obs))
    for (k in 1:(length(ep)-1)) {
      cum.inflow[(ep[k]):(ep[k+1])] <- cumsum(inflow[(ep[k]):(ep[k+1])])
    }
    cum.inflow <- cum.inflow*sec.per.day
  }

  max.vol <- max(cum.sim,cum.obs,cum.inflow,na.rm=T)*1.1

  plot(lubridate::date(sim),cum.sim,type='l',col='red',ylim=c(0,max.vol),
       ylab='Cumulative Flow [m3]',xlab='Date',lty=1)

  if (!(is.null(obs))) {
    lines(lubridate::date(obs),cum.obs,col='black',lty=3)
  }
  if (!(is.null(inflow))) {
    lines(lubridate::date(inflow),cum.inflow,col='blue',lty=5)
  }

# add a legend
  if (is.null(inflow) & is.null(obs) ) {
    # legend for only sim
    legend(
      x='topleft',
      c('sim'),
      col=c('red'),
      cex=0.7,inset=0.01,
      lty=c(1)
    )
  } else if ( is.null(inflow) & !(is.null(obs))) {
    # legend for sim and obs
    legend(
      x='topleft',
      c('sim','obs'),
      col=c('red','black'),
      cex=0.7,inset=0.01,
      lty=c(1,3)
    )
  } else if ( !(is.null(inflow)) & is.null(obs)) {
    # legend for sim and inflow
    legend(
      x='topleft',
      c('sim','inflow'),
      col=c('red','blue'),
      cex=0.7,inset=0.01,
      lty=c(1,5)
    )
  } else {
    # legend for all 3
    legend(
      x='topleft',
      c('sim','obs','inflow'),
      col=c('red','black','blue'),
      cex=0.7,inset=0.01,
      lty=c(1,3,5)
    )
  }
  return(TRUE)
}
