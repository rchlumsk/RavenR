#' @title Annual Volume Comparison
#'
#' @description
#' rvn_annual_volume creates a plot of the annual observed and simulated volumes.
#'
#' @details
#' This function creates a scatterplot of the annual observed and simulated
#' volumes, calculated for each available water year of data within the two series provided.
#' The sim and obs should be of time
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
#' @param mm month of water year ending (default 9)
#' @param dd day of water year ending (default 30)
#' @param add_line optionally adds a 1:1 line to the plot for reference (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param add_eqn optionally adds the equation for a linear regression line through the origin (default FALSE)
#' @param add_labels optionally adds year-ending labels to each point on plot using geom_text (default FALSE)
#' @return returns a list with annual volume data in a data frame, and a ggplot object
#'  \item{df_volume}{data frame of the calculated annual volumes}
#'  \item{p1}{ggplot object with plotted annual volumes}
#' @seealso \code{\link{rvn_flow_scatterplot}} to create a scatterplot of flow
#' values
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # create a plot of the annual volumes with defaults
#' rvn_annual_volume(sim,obs)
#'
#' # create a plot of the annual volumes with r2
#' rvn_annual_volume(sim,obs,add_r2=TRUE, add_eqn=TRUE)
#'
#' # create a plot of the annual volumes with year-ending labels
#' rvn_annual_volume(sim,obs, add_labels=TRUE)
#'
#' # calculate annual volumes for different water years (e.g. ending Oct 31)
#' vv <- rvn_annual_volume(sim, obs, mm=10, dd=31)
#' vv$df.volume
#' vv$p1
#'
#' @export rvn_annual_volume
#' @importFrom stats lm
#' @importFrom lubridate year date
#' @importFrom ggplot2 ggplot aes geom_point geom_abline geom_text scale_x_continuous scale_y_continuous
rvn_annual_volume <- function (sim, obs, mm=9, dd=30, add_line = TRUE, add_r2 = FALSE, add_eqn=FALSE, add_labels=FALSE)
{
  sec.per.day <- 86400

  sum.sim <- rvn_apply_wyearly(sim, sum, mm=mm, dd=dd, na.rm = TRUE)
  dates <- lubridate::date(sum.sim)
  sum.sim <- as.numeric(sum.sim)

  sum.obs <- rvn_apply_wyearly(obs, sum, mm=mm, dd=dd, na.rm = TRUE) %>%
    as.numeric()

  sum.sim <- sum.sim * sec.per.day
  sum.obs <- sum.obs * sec.per.day
  df <- data.frame(date.end = dates, sim.vol = sum.sim, obs.vol = sum.obs)

  x.lab <- expression("Observed Volume ("*m^3*")")
  y.lab <- expression("Simulated Volume ("*m^3*")")
  title.lab <- ""
  x.lim = c(min(sum.obs, sum.sim, na.rm = TRUE) * 0.9,
            max(sum.obs, sum.sim, na.rm = TRUE) * 1.1)
  y.lim = c(min(sum.obs, sum.sim, na.rm = TRUE) * 0.9,
            max(sum.obs, sum.sim, na.rm = TRUE) * 1.1)

  obs.vol <- sim.vol <- date.end <- NULL

  p1 <- ggplot(data=df,aes(x=obs.vol,y=sim.vol))+
    geom_point()+
    scale_x_continuous(limits=x.lim, name=x.lab)+
    scale_y_continuous(limits=y.lim, name=y.lab)+
    rvn_theme_RavenR()

  if (add_line){
    p1 <- p1 +
      geom_abline(linetype=2)
  }

  if (add_r2 | add_eqn) {
    m <- lm(sum.sim ~ 0 + sum.obs)
  }

  if (add_r2){
    r2.label <- paste("R^2 == ", round(summary(m)$r.squared,3))
    p1 <- p1 +
      geom_text(x= x.lim[2]*0.85,
                y= y.lim[1]*1.1,
                vjust = 1,
                hjust = 0,
                label=r2.label,
                parse=TRUE,
                size = 3.5)
  }

  if (add_eqn) {
    coeff <- round(as.numeric(m$coefficients[1]), 3)
    equation <- paste0( "y = ", coeff, "x")
    p1 <- p1 +
      geom_text(x= x.lim[2]*0.85,
                y= y.lim[1]*1.2,
                vjust = 1,
                hjust=0,
                label = equation,
                size = 3.5)
  }

  if (add_labels) {
    p1 <- p1 + geom_text(data=df, aes(label=lubridate::year(date.end)), nudge_y=max(df[,-1])/25, fontface='bold')
  }

  return(list(df.volume=df,p1=p1))
}
