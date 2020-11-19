#' @title Scatterplot of model flows
#'
#' @description
#' rvn_flow_scatterplot creates a scatterplot of the simulated and observed flows.
#' Useful in diagnotic analysis of model outputs.
#'
#' @details
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
#' @param add_line optionally adds a 1:1 line to the plot for reference (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param add_eqn optionally adds the equation for a linear regression line (default FALSE)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{rvn_forcings_read}} for reading in the ForcingFunctions
#' file
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_ob
#'
#' # plot the flow scatterplot, produce an R2 metric
#' rvn_flow_scatterplot(sim,obs,add_r2=TRUE)
#'
#' # plot again with a regression equation
#' rvn_flow_scatterplot(sim,obs,add_r2=TRUE,add_eqn=TRUE)
#'
#' @export rvn_flow_scatterplot
#' @importFrom ggplot2 ggplot geom_point scale_x_continuous scale_y_continuous geom_abline geom_text
rvn_flow_scatterplot <- function(sim,obs,add_line=TRUE,add_r2=FALSE, add_eqn = FALSE)
{
  x.lab <- expression("Observed Flow ("*m^3*"/s)")
  y.lab <- expression("Simulated Flow ("*m^3*"/s)")

  plot.df <- fortify(cbind(sim,obs))
  colnames(plot.df) <- c("date","sim","obs")
  max.flow <- max(obs, sim, na.rm = TRUE)

  p1 <- ggplot(plot.df)+
    geom_point(aes(x=obs,y=sim))+
    scale_x_continuous(name=x.lab,limits=c(0,max.flow))+
    scale_y_continuous(name=y.lab,limits=c(0,max.flow))+
    rvn_theme_RavenR()

  if (add_line){
    p1 <- p1+
      geom_abline(linetype="dashed")
  }

  if (add_r2 | add_eqn) {
    m <- lm(sim ~ 0 + obs)
  }

  if (add_r2) {
    r2.label <- paste("R^2 == ", round(summary(m)$r.squared,3))
    p1 <- p1 +
      geom_text(x= max(obs, sim, na.rm = TRUE)*0.9,
                y= max(obs, sim, na.rm = TRUE)*0.18,
                vjust = 1,
                label=r2.label,
                parse=T,
                size = 3.5)

    if (add_eqn){
      coeff <- round(as.numeric(m$coefficients[1]), 3)
      # equation <- paste0( "y = ", coeff[2], "x + ", coeff[1])
      equation <- paste0( "y = ", coeff, "x ")
      p1 <- p1 +
        geom_text(x = max(obs, sim, na.rm = TRUE)*0.9,
                  y = max(obs, sim, na.rm = TRUE)*0.1,
                  vjust = 1,
                  label = equation,
                  size = 3.5)
    }
  }

  return(p1)
}
