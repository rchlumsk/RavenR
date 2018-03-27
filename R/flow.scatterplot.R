#' Scatterplot of model flows
#'
#' flow.scatterplot creates a scatterplot of the simulated and observed flows.
#' Useful in diagnotic analysis of model outputs.
#'
#' This function creates a scatterplot of flows.
#'
#' The sim and obs should be of time series (xts) format. The flow series are
#' assumed to be daily flows with units of m3/s.
#'
#' The R2 diagnostic is calculated for a fit with no intercept (in a perfect
#' fit the points are identical, and intercept is automatically zero). The R2
#' is calculated with the NA values removed
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param add.line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add.r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param axis.zero optionally sets the minimum flow on axes to zero (default
#' TRUE)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{forcings.read}} for reading in the ForcingFunctions
#' file
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow scatterplot diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_ob
#'
#' # plot the flow scatterplot, produce an R2 metric
#' flow.scatterplot(sim,obs,add.r2=T)
#'
#' @export flow.scatterplot
flow.scatterplot <- function(sim,obs,add.line=T,add.r2=F,axis.zero=T) {
  x.lab <- "Observed flow [m3/s]"
  y.lab <- "Simulated flow [m3/s]"
  max.flow <- max(obs, sim,na.rm=T)
  if (axis.zero) {
    min.flow <- 0
  } else {
    min.flow <- min(obs, sim,na.rm=T)
  }
  plot(coredata(obs), coredata(sim), xlim=c(min.flow,max.flow), ylim=c(min.flow,max.flow),
       xlab=x.lab, ylab=y.lab)
  if (add.line) {
    abline(0,1, lty=2)
  }
  if (add.r2) {
    # check the calculation to ensure it is R2 with no intercept
    r2 <- 1 - (sum((obs-sim)^2,na.rm=T)/sum((obs-mean(obs,na.rm=T))^2,na.rm=T))
    mtext(sprintf('R2 = %.2f',r2), side=3,adj=1)
  }

  return(TRUE)
}
