#' @title Flow Spaghetti Plot
#'
#' @description
#' rvn_flow_spaghetti creates a spaghetti plot of the flow series provided.
#'
#' @details
#' This function creates a spaghetti plot of the annual flow series in each
#' year of data provided. The flows are plotted for each water year of data
#' available, set as October 1st.
#'
#' Note that the plotting to the day of year is approximate in order to
#' simplify the plotting of leap years and non-leap years. The years are
#' plotted including day 366 and starting on day 274, regardless of whether it
#' is a leap year or not. This is likely without consequence in seeing the
#' trends between water years, however the user is warned of this deficiency.
#'
#' The flow series provided should be of time series (xts) format.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param flow time series object of simulated flows
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{rvn_flow_scatterplot}} to create a scatterplot of flow
#' values
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#'
#' # create spaghetti plot of simulated flows
#' rvn_flow_spaghetti(rvn_hydrograph_data$hyd$Sub36)
#'
#' # create spaghetti plot of observed flows
#' rvn_flow_spaghetti(rvn_hydrograph_data$hyd$Sub36_obs)
#'
#' @export rvn_flow_spaghetti
#' @importFrom lubridate yday
#' @importFrom ggplot2 ggplot geom_line scale_y_continuous scale_x_continuous aes fortify
rvn_flow_spaghetti <- function(flow)
{

  x_form <- Year <- NULL

  ticks.at <- seq(1, 366, 1)
  ticks.seq <- c(seq(274, 366, 1), seq(1, 273, 1))

  plot.df <- fortify(flow)

  plot.df$doy <- lubridate::yday(plot.df$Index)
  plot.df$Year <- as.factor(year(plot.df$Index))
  colnames(plot.df)[2] <- "flow"
  plot.df$x_form <- plot.df$doy-273
  plot.df$x_form[month(plot.df$Index)<10] <- plot.df$x_form[month(plot.df$Index)<10]+365

  labels <- c(seq(274,365,30),seq(1,270,30))

  p1 <- ggplot(plot.df)+
    geom_line(aes(x=x_form,y=flow,group=Year, color=Year))+
    scale_y_continuous(name=expression("Flow ("*m^3*"/s)"))+
    scale_x_continuous(name="Day of Year", breaks=seq(1,365,30), labels = labels)+
    rvn_theme_RavenR()

  return(p1)
}

