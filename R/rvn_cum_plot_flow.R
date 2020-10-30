#' @title Cumulative Plot of model flows
#'
#' @description
#' rvn_cum_plot_flow creates a cumulative flow plot of the simulated flows;
#' optionally includes an observed and/or inflow series as well. Useful in
#' diagnotic analysis of model outputs.
#'
#' @details
#' This function will plot the simulated series in all cases, and will include
#' the observed and inflow plots if they are supplied.
#'
#' The sim and obs should be of time series (xts) format. The flow series are
#' assumed to be daily flows with units of m3/s.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' Note that the cumsum function does not have an na.rm=T argument, thus if
#' there are any NA values in the water year of data for any provided series,
#' the values beyond an NA value will be calculated as NA. It is up to the user
#' to handle NA values appropriately fill in or replace NA values based on the
#' type of data supplied. For flow series, linear interpolation for small periods
#' of missing values may be appropriate.
#'
#' @param sim time series object of simulated flows
#' @param obs optionally supply an inflow series to plot as well
#' @param inflow optionally supply an inflow series to plot as well
#' @param mm month of water year ending (default 9)
#' @param dd day of water year ending (default 30)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{rvn_flow_scatterplot}} for creating flow scatterplots
#' @seealso \code{\link{rvn_cum_plot_flow}} for creating generic cumulative function plotting
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # plot cumulative flow for sim and obs
#' rvn_cum_plot_flow(sim,obs)
#'
#' # plot cumulative flows for specific period
#' prd <- "2003-10-01/2004-10-01"
#' rvn_cum_plot_flow(sim[prd],obs[prd])
#'
#' @export rvn_cum_plot_flow
#' @importFrom ggplot2 fortify geom_line aes scale_y_continuous xlab ylab scale_colour_brewer
rvn_cum_plot_flow <- function(sim=NULL,obs=NULL,inflow=NULL, mm=9, dd=30)
{

  sec.per.day <- 86400
  cum.sim <- NULL ; cum.obs <- NULL ; cum.inflow <- NULL

  variable <- cum <- NULL

  # get the indicies of water years
  ep <- rvn_wyear_indices(sim, mm=mm, dd=dd)

  cum.sim <- matrix(NA,nrow=nrow(sim))
  for (k in 1:(length(ep)-1)) {
    cum.sim[(ep[k]):(ep[k+1])] <- cumsum(sim[(ep[k]):(ep[k+1])])
  }
  cum.sim <- cum.sim*sec.per.day

  if (!(is.null(obs))) {
    cum.obs <- matrix(NA,nrow=nrow(obs))
    for (k in 1:(length(ep)-1)) {
      cum.obs[(ep[k]):(ep[k+1])] <- cumsum(obs[(ep[k]):(ep[k+1])])
    }
    cum.obs <- cum.obs*sec.per.day
  }

  if (!(is.null(inflow))) {
    cum.inflow <- matrix(NA,nrow=nrow(obs))
    for (k in 1:(length(ep)-1)) {
      cum.inflow[(ep[k]):(ep[k+1])] <- cumsum(inflow[(ep[k]):(ep[k+1])])
    }
    cum.inflow <- cum.inflow*sec.per.day
  }

  max.vol <- max(cum.sim,cum.obs,cum.inflow,na.rm=TRUE)*1.1

  df.plot <- data.frame(cbind(fortify(sim),cum.sim,"sim"))
  colnames(df.plot) <- c("date","value","cum","variable")

  if (!(is.null(obs))) {
    df <- cbind(fortify(obs),cum.obs,"obs")
    colnames(df) <- c("date","value","cum","variable")
    df.plot <- rbind(df.plot,df)
  }
  if (!(is.null(inflow))) {
    df <- cbind(fortify(inflow),cum.obs,"inflow")
    colnames(df) <- c("date","value","cum","variable")
    df.plot <- rbind(df.plot,df)
  }

  df.plot$cum <- as.numeric(df.plot$cum)
  df.plot$variable <- factor(df.plot$variable)

  cum_flow_plot <- ggplot(df.plot)+
    geom_line(aes(x=date,y=cum,color=variable))+
    scale_y_continuous(limits=c(0,max.vol))+
    xlab("Date")+
    ylab(expression("Cummulative Volume ("*m^3*")"))+
    scale_colour_brewer(type = "qual", palette = 3) +
    rvn_theme_RavenR()

  return(cum_flow_plot)
}
