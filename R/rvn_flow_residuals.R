#' @title Residuals of model flows
#'
#' @description
#' rvn_flow_residuals creates a residuals time series for flow values. Useful in
#' diagnotic analysis of model outputs.
#'
#' @details
#' This function creates a residuals time series plot for flow values, with the
#' option to smooth out the values using the rollmean function in zoo. The
#' winter months are optionally shaded in the time series; winter period is
#' defined as December 1st to March 31st.
#'
#' The residuals are calculated as sim - obs.
#'
#' The sim and obs should be of time series (xts) format. The flow series are
#' assumed to be daily flows with units of m3/s.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' The winter_shading argument will add a transparent grey shading for the
#' specified period by wsdates in each year that is plotted.
#'
#' wsdates is formatted as c(winter start month, winter start day, winter end month, winter end day).
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param ma_smooth optional length of rolling average to smooth residuals with
#' (default 3)
#' @param add_line optionally adds a horizontal line to the plot for reference
#' (default FALSE)
#' @param winter_shading optionally adds a light blue shading to winter months
#' (default FALSE)
#' @param wsdates integer vector of winter shading period dates (see details)
#' @return \item{resids}{residual time series}
#'
#' @seealso \code{\link{rvn_flow_scatterplot}} to create a scatterplot of flow
#' values
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' ff <- system.file("extdata/run1_Hydrographs.csv", package="RavenR")
#' run1 <- rvn_hyd_read(ff)
#' sim <- run1$hyd$Sub36
#' obs <- run1$hyd$Sub36_obs
#'
#' # default with moving average smoothing shading of winter months
#' rvn_flow_residuals(sim,obs)$plot
#'
#' # plot with more smoothing than the default 3
#' rvn_flow_residuals(sim, obs, ma_smooth=10)$plot
#'
#' # with zero line and winter shading
#' rvn_flow_residuals(sim,obs, add_line=TRUE, winter_shading = TRUE)$plot
#'
#' # change winter shading to Nov 1 - April 30
#' rvn_flow_residuals(sim,obs, add_line=TRUE,
#'   winter_shading = TRUE, wsdates=c(11,1,4,30))$plot
#'
#' @export rvn_flow_residuals
#' @importFrom zoo rollapply
#' @importFrom ggplot2 fortify ggplot geom_line ylab geom_hline geom_rect aes
rvn_flow_residuals <- function(sim=NULL,obs=NULL, ma_smooth=3, add_line=FALSE, winter_shading=FALSE, wsdates=c(12,1,3,31))
{

  if ( ma_smooth < 0) {
    stop("Requires a non-negative integer for ma_smooth ")
  }

  if (is.null(sim) | is.null(obs)) {
    stop("Requires non-null sim and obs series.")
  }

  if  (ma_smooth == 0 ) {
    resids <- sim-obs
  } else {
    resids <- rollapply(sim-obs,ma_smooth,mean,fill=NA)
  }

  y.end <- y.start <- Date <- Resid <- NULL

  df.plot <- fortify(resids)
  colnames(df.plot) <- c("Date","Resid")
  df.plot$Date <- as.Date(df.plot$Date)

  p1 <- ggplot(df.plot)+
    geom_line(aes(x=Date,y=Resid))+
    ylab("Smoothed Residual")+
    rvn_theme_RavenR()

  if (add_line){
    p1 <- p1 +
      geom_hline(yintercept=0,linetype="dashed")
  }

  if (winter_shading) {

    winter.start <- as.Date(df.plot$Date[month(df.plot$Date) == wsdates[1] & day(df.plot$Date) == wsdates[2]],
                            origin = "1970-01-01")
    winter.end <- as.Date(df.plot$Date[month(df.plot$Date) == wsdates[3] & day(df.plot$Date) == wsdates[4]],
                          origin = "1970-01-01")

    shade <- data.frame(winter.start,winter.end)
    shade$y.start <- -Inf
    shade$y.end <- Inf

    p1 <- p1 +
      geom_rect(data = shade, aes(xmin=winter.start,xmax=winter.end,ymin=y.start,ymax=y.end),color="grey50",alpha=0.1, linetype=0)
  }

  resids <- list(resids = resids, plot = p1)

  return(resids)
}
