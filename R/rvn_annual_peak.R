#' @title Annual Peak Comparison
#'
#' @description
#' rvn_annual_peak creates a plot of the annual observed and simulated peaks, based on the water year.
#'
#' @details
#' This function creates a scatterplot of the annual observed and simulated
#' peaks, calculated for each available water year of data
#' within the two series provided. The default start of the water year
#' is October 1st, but may be adjusted through function parameters. Note that the
#' calculation uses the peak magnitude of simulated and observed series in each water year, without
#' considering the timing of the events in each series.
#'
#' The sim and obs should be of time series (xts) format and are assumed to be
#' of the same length and time period. The flow series are assumed to be daily
#' flows with units of m3/s.
#'
#' The R2 diagnostic is calculated for a fit with no intercept, consistent with the provided
#' 1:1 line (in a perfect fit the points are identical, and intercept is automatically zero).
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param mm month of water year ending (default 9)
#' @param dd day of water year ending (default 30)
#' @param add_line optionally adds a 1:1 line to the plot for reference (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param add_eqn optionally adds the equation for a linear regression line through the origin (default FALSE)
#' @return returns a list with peak data in a data frame, and a ggplot object
#'  \item{df_peak}{data frame of the calculated peaks}
#'  \item{p1}{ggplot object with plotted annual peaks}
#'
#' @seealso \code{\link{rvn_annual_volume}} to create a scatterplot of annual flow
#' volumes \code{\link{rvn_annual_peak_event}} to consider the timing of peak
#' events.
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of annual peaks with default options
#' peak1 <- rvn_annual_peak(sim, obs)
#' peak1$df_peak
#' peak1$p1
#'
#' # plot with r2 and regression equation
#' peak_df <- rvn_annual_peak(sim, obs, add_r2=TRUE, add_eqn=TRUE)
#' peak_df$p1
#'
#' @export rvn_annual_peak
#' @importFrom stats lm
#' @importFrom lubridate year date
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_text scale_x_continuous scale_y_continuous
rvn_annual_peak <- function(sim, obs, mm=9, dd=30, add_line = TRUE,
                             add_r2 = FALSE, add_eqn = FALSE)
{

  max.sim <- rvn_apply_wyearly_which_max_xts(sim, mm=mm, dd=dd)
  max.obs <- rvn_apply_wyearly_which_max_xts(obs, mm=mm, dd=dd)
  dates <- lubridate::date(sim[rvn_wyear_indices(sim, mm=mm, dd=dd)])
  df <- data.frame("date.end" = dates,
                   "sim.peak" = as.numeric(max.sim),
                   "obs.peak" = as.numeric(max.obs))

  x.lab <- expression("Observed Peak Discharge ("*m^3*"/s)")
  y.lab <- expression("Simulated Peak Discharge ("*m^3*"/s)")
  x.lim = c(min(max.obs, max.sim, na.rm= TRUE) * 0.9,
            max(max.obs, max.sim, na.rm= TRUE) * 1.1)
  y.lim = c(min(max.obs, max.sim, na.rm= TRUE) * 0.9,
            max(max.obs, max.sim, na.rm= TRUE) * 1.1)

  p1 <- ggplot(data=df,aes(x=max.obs,y=max.sim))+
    geom_point()+
    scale_x_continuous(limits=x.lim, name=x.lab)+
    scale_y_continuous(limits=y.lim, name=y.lab)+
    rvn_theme_RavenR()

  if (add_line){
    p1 <- p1 +
      geom_abline(linetype=2)
  }

  if (add_r2 | add_eqn) {
    m <- lm(max.sim ~ 0 + max.obs)
  }

  if (add_r2){
    r2.label <- paste("R^2 == ", round(summary(m)$r.squared,3))
    p1 <- p1 +
      geom_text(x= max(max.obs, max.sim),
                y= min(max.sim, max.obs),
                vjust = 1,
                label=r2.label,
                parse=T,
                size = 3.5)
  }

  if (add_eqn) {
    coeff <- round(as.numeric(m$coefficients[1]), 3)
    equation <- paste0( "y = ", coeff, "x")
    p1 <- p1 +
      geom_text(x = max(max.obs, max.sim),
                y = min(max.sim, max.obs)*1.1,
                vjust = 1,
                label = equation,
                size = 3.5)
  }

  return(list(df_peak = df,p1=p1))
}
