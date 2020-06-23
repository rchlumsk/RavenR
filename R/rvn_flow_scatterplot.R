#' Scatterplot of model flows
#'
#' rvn_flow_scatterplot creates a scatterplot of the simulated and observed flows.
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
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
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
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_ob
#'
#' # plot the flow scatterplot, produce an R2 metric
#' rvn_flow_scatterplot(sim,obs,add_r2=T)
#'
#' @export rvn_flow_scatterplot
rvn_flow_scatterplot <- function(sim,obs,add_line=T,add_r2=F) {
  x.lab <- expression("Observed Flow ["*m^3*"/s]")
  y.lab <- expression("Simulated Flow ["*m^3*"/s]")

  plot.df <- fortify(cbind(sim,obs))
  colnames(plot.df) <- c("date","sim","obs")
  max.flow <- max(obs, sim, na.rm = T)

  p1 <- ggplot(plot.df)+
    geom_point(aes(x=obs,y=sim))+
    scale_x_continuous(name=x.lab,limits=c(0,max.flow))+
    scale_y_continuous(name=y.lab,limits=c(0,max.flow))+
    theme_bw()

  if (add.line){
    p1 <- p1+
      geom_abline(linetype="dashed")
  }

  if (add.r2) {
    r2 <- 1 - (sum((obs - sim)^2, na.rm = T)/sum((obs - mean(obs,
                                                             na.rm = T))^2, na.rm = T))
    r2.label <- paste("R^2 == ", round(r2,2))
    p1 <- p1 +
      annotate(geom="text",x=max.flow*0.9,y=max.flow*0.9,label=r2.label, parse=T)
  }

  return(p1)
}
