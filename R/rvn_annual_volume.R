#' Annual Volume Comparison
#'
#' rvn_annual_volume creates a plot of the annual observed and simulated volumes.
#'
#' This function creates a scatterplot of the annual observed and simulated
#' volumes, calculated for each available water year of data (Oct 1st
#' hardcoded) within the two series provided. The sim and obs should be of time
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
#' @param rplot boolean whether to generate plot (default TRUE)
#' @param add_line optionally adds a 1:1 line to the plot for reference
#' (default TRUE)
#' @param add_r2 optionally computes the R2 and adds to plot (default FALSE)
#' @param axis_zero optionally sets the minimum volume on axes to zero (default
#' FALSE)
#' @return \item{df_volume}{data frame of the calculated annual volumes}
#' @seealso \code{\link{flow_scatterplot}} to create a scatterplot of flow
#' values
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven annual volume diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph_data)
#' sim <- hydrograph_data$hyd$Sub36
#' obs <- hydrograph_data$hyd$Sub36_o
#'
#' # create a plot of the annual volumes with defaults
#' rvn_annual_volume(sim,obs)
#'
#' # create a plot of the annual volumes with r2 and axis set to zero
#' rvn_annual_volume(sim,obs,add_r2=T,axis_zero=T)
#'
#' # store results of annual volumes
#' volumes <- rvn_annual_volume(sim,obs,rplot=F)
#'
#' @export rvn_annual_volume
rvn_annual_volume <- function (sim, obs, rplot = T, add_line = T, add_r2 = F) {
  sec.per.day <- 86400
  sum.sim <- apply.wyearly(sim, sum, na.rm = T)
  dates <- sum.sim[, 1]
  sum.sim <- sum.sim[, 2]
  sum.obs <- apply.wyearly(obs, sum, na.rm = T)[, 2]
  sum.sim <- sum.sim * sec.per.day
  sum.obs <- sum.obs * sec.per.day
  df <- data.frame(date.end = dates, sim.vol = sum.sim, obs.vol = sum.obs)
  if (add_r2) {
    sum.obs.mean <- mean(sum.obs)
    ss.err <- sum((sum.sim - sum.obs)^2)
    ss.tot <- sum((sum.obs - sum.obs.mean)^2)
    r2 <- 1 - ss.err/ss.tot
  }
  if (rplot) {
    x.lab <- expression("Observed Volume ["*m^3*"]")
    y.lab <- expression("Simulated Volume ["*m^3*"]")
    title.lab <- ""
    x.lim = c(min(sum.obs, sum.sim, na.rm = T) * 0.9,
              max(sum.obs, sum.sim, na.rm = T) * 1.1)
    y.lim = c(min(sum.obs, sum.sim, na.rm = T) * 0.9,
              max(sum.obs, sum.sim, na.rm = T) * 1.1)

    text.labels <- year(dates)

    #Base Plot
    p1 <- ggplot(data=df,aes(x=obs.vol,y=sim.vol,label=text.labels))+
      geom_point()+
      geom_text(hjust=0.5,vjust=-0.5)+
      scale_x_continuous(limits=x.lim, name=x.lab)+
      scale_y_continuous(limits=y.lim, name=y.lab)+
      theme_bw()

    if (add_line){
      p1 <- p1 +
        geom_abline(linetype=2)
    }

    if (add_r2){
      r2.label <- paste("R^2 == ", round(r2,2))
      p1 <- p1 +
        annotate(geom="text",x=(x.lim[2]-x.lim[1])*0.5+x.lim[1],y=y.lim[2],label=r2.label, parse=T)

    }
    return(list(df.volume=df,plot=p1))
  } else{

    return(df.volume=df)
  }
}
