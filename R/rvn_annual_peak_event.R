#' @title Annual Peak Event Comparison
#'
#' @description
#' rvn_annual_peak_event creates a plot of the annual observed and simulated peaks,
#' based on the water year.
#'
#' @details
#' This function creates a scatterplot of the annual observed and simulated
#' peaks, calculated for each available water year of data
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
#' @param mm month of water year (default 9)
#' @param dd day of water year (default 30)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param add_eqn optionally adds the equation for a linear regression line through the origin (default FALSE)
#' @return returns a list with peak data in a data frame, and a ggplot object
#'  \item{df_peak_event}{data frame of the calculated peak events}
#'  \item{p1}{ggplot object with plotted annual peaks}
#'
#' @seealso \code{\link{rvn_annual_peak}} to create a scatterplot of annual peaks
#' (consider the magnitude of peaks only)
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of annual peak events with default options
#' peak1 <- rvn_annual_peak_event(sim, obs)
#' peak1$df_peak_event
#' peak1$p1
#'
#' @export rvn_annual_peak_event
#' @importFrom stats lm
#' @importFrom lubridate year date
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_text scale_x_continuous scale_y_continuous
rvn_annual_peak_event <- function (sim, obs, mm=9, dd=30, add_line = TRUE, add_r2 = FALSE, add_eqn = FALSE)
{

  max.obs <- rvn_apply_wyearly_which_max_xts(obs, mm=mm, dd=dd)
  max.dates <- lubridate::date(max.obs)
  max.sim <- sim[max.dates]

  max.sim <- as.numeric(max.sim)
  max.obs <- as.numeric(max.obs)

  df <- data.frame(matrix(NA, nrow=length(max.dates), ncol=3))
  df[,1] <- max.dates
  df[,2] <- max.sim
  df[,3] <- max.obs
  colnames(df) <- c("obs.dates","sim.peak.event","obs.peak.event")

  # df <- data.frame("obs.dates" = max.dates,
  #                  "sim.peak.event" = as.numeric(max.sim),
  #                  "obs.peak.event" = as.numeric(max.obs))

  x.lab <- expression("Observed Peak Discharge ("*m^3*"/s)")
  y.lab <- expression("Simulated Peak Discharge ("*m^3*"/s)")
  #title.lab <- ""

  x.lim = c(min(max.obs, max.sim, na.rm = T) * 0.9,
            max(max.obs, max.sim, na.rm = T) * 1.1)
  y.lim = c(min(max.obs, max.sim, na.rm = T) * 0.9,
            max(max.obs, max.sim, na.rm = T) * 1.1)

  #text.labels <- year(max.dates)

  # appease R CMD CHECK with ggplot2
  sim.peak.event <- obs.peak.event <- obs.dates <- NULL

  p1 <- ggplot(data=df,aes(x=obs.peak.event,y=sim.peak.event))+
    geom_point()+
    scale_x_continuous(limits=x.lim, name=x.lab)+
    scale_y_continuous(limits=y.lim, name=y.lab)+
    rvn_theme_RavenR()

  if (add_line){
    p1 <- p1 +
      geom_abline(linetype=2)
  }

  if (add_r2 | add_eqn) {
    m <- lm(max.sim ~ max.obs + 0)
  }

  if (add_r2){
    r2.label <- paste("R^2 == ", round(summary(m)$r.squared,3))
    p1 <- p1 +
      geom_text(x= x.lim[2],
                y= y.lim[1]*1.9,
                vjust = 1,
                hjust = 1,
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

  return(list(df_peak_event = df,p1=p1))
}
