#' Scatterplot of model flows
#'
#' rvn_flow_residuals creates a residuals time series for flow values. Useful in
#' diagnotic analysis of model outputs.
#'
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
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param ma_smooth optional length of rolling average to smooth residuals with
#' (default 3)
#' @param add.line optionally adds a horizontal line to the plot for reference
#' (default TRUE)
#' @param winter_shading optionally adds a light blue shading to winter months
#' (default TRUE)
#' @return \item{resids}{residual time series}
#' @seealso \code{\link{flow_scatterplot}} to create a scatterplot of flow
#' values
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow residual diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_ob
#'
#' # default with moving average smoothing shading of winter months
#' rvn_flow_residuals(sim,obs)
#'
#' # plot with more smoothing than the default 3
#' rvn_flow_residuals(sim, obs, ma_smooth=10)
#'
#' # turn off the smoothing and winter shading
#' rvn_flow_residuals(sim,obs,ma_smooth = 0, winter_shading = F)
#'
#' @export rvn_flow_residuals
rvn_flow_residuals <- function(sim,obs,ma_smooth=3,add.line=T,winter_shading=T) {

  if ( ma_smooth < 0) {
    stop("Requires a non-negative integer for ma_smooth ")
  }

  if  (ma_smooth == 0 ) {
    resids <- sim-obs
  } else {
    resids <- rollapply(sim-obs,ma_smooth,mean,fill=NA)
  }

  df.plot <- fortify(resids)
  colnames(df.plot) <- c("Date","Resid")
  df.plot$Date <- as.Date(df.plot$Date)
  
  p1 <- ggplot(df.plot)+
    geom_line(aes(x=Date,y=Resid))+
    ylab("Smoothed Residual")+
    theme_bw()
  
  if (add.line){
    p1 <- p1 +
      geom_hline(yintercept=0,linetype="dashed")
  }
  
  if (winter.shading){
    
    
    winter.start <- as.Date(df.plot$Date[month(df.plot$Date) == 12 & day(df.plot$Date) == 1])
    winter.end <- as.Date(df.plot$Date[month(df.plot$Date) == 3 & day(df.plot$Date) == 31])
    
    shade <- data.frame(cbind(winter.start,winter.end))
    shade$winter.start <- as.Date(shade$winter.start)
    shade$winter.end <- as.Date(shade$winter.end)
    shade$y.start <- -Inf
    shade$y.end <- Inf
    
    p1 <- p1 + 
      geom_rect(data = shade, aes(xmin=winter.start,xmax=winter.end,ymin=y.start,ymax=y.end),color="grey",alpha=0.3)
    
    
  }
  
  resids <- list(resids = resids, plot = p1)
  
  return(resids)
}
