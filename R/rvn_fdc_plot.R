#' @title Plots summary of watershed forcing functions
#'
#' @description
#' rvn_fdc_plot generation a flow duration curve plot.
#'
#' @details
#' This function creates a flow duration curve using the rvn_hyd_extract obejct for
#' a given basin. The hydrograph object passed should be the output from the
#' rvn_hyd_extract function, which has attributes for sim and obs; if the obs is
#' NULL, only the sim FDC will be plotted.
#'
#' If the seasonal argument is included, the winter and summer FDC lines will
#' be included on the plot as well.
#'
#' @param sim simulated hydrograph xts time series
#' @param obs (optional) observed hydrograph xts time series
#' @param prd (optional) time period over which the plot is generated
#' @param seasonal (optional) boolean whether to add the winter and summer FDC
#'
#' @seealso \code{\link{rvn_hyd_read}} for reading in the Hydrographs.csv file, and
#' \code{\link{rvn_hyd_extract}} for extracting basin flow information from a
#' rvn_hyd_read object
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' ff <- system.file("extdata/run1_Hydrographs.csv", package="RavenR")
#' run1 <- rvn_hyd_read(ff)
#' sim <- run1$hyd$Sub36
#' obs <- run1$hyd$Sub36_obs
#'
#' # create FDC plot, sim only
#' rvn_fdc_plot(sim)
#'
#'  # create seasonal FDC plot with sim and obs data
#' rvn_fdc_plot(sim,obs,seasonal=TRUE)
#'
#' @export rvn_fdc_plot
#' @importFrom ggplot2 fortify ggplot scale_x_continuous scale_y_continuous scale_colour_brewer stat_ecdf
rvn_fdc_plot <-function(sim=NULL,obs=NULL,prd=NULL,seasonal=FALSE){

  if (is.null(sim)) {
    stop("sim is required for plotting.")
  }

  Value <- Type <- NULL

  prd <- rvn_get_prd(sim, prd)

  summer=NULL
  winter=NULL
  if (seasonal!='F'){
    sim.month<-month(sim,label=F,abbr=F)
    summer<-sim[sim.month >=5 & sim.month<9] # May to September
    winter<-sim[sim.month <5 | sim.month>=9]
  }

  if (!(is.null(obs))) {
    obs=sim+rep(10,length(sim)); # temp debug
  }

  xmax=10^(ceiling(log10(max(sim))));
  xmin=10^(floor(log10(min(sim))));

  plot.df <- fortify(sim)
  plot.df$Type <- "Sim"
  colnames(plot.df)[2] <- "Value"

  if (!is.null(obs)){
    plot.df2 <- fortify(obs)
    plot.df2$Type <- "Obs"
    colnames(plot.df2)[2] <- "Value"
    plot.df <- rbind(plot.df, plot.df2)
  }

  if (seasonal){
    plot.df3 <- fortify(summer)
    plot.df3$Type <- "Summer"
    colnames(plot.df3)[2] <- "Value"

    plot.df4 <- fortify(winter)
    plot.df4$Type <- "Winter"
    colnames(plot.df4)[2] <- "Value"

    plot.df <- rbind(plot.df, plot.df3,plot.df4)
  }

  p1 <-  ggplot(plot.df)+
    stat_ecdf(aes(x=Value, color=Type))+
    scale_x_continuous(trans = 'log10',
                       name = expression("Daily Discharge ("*m^3*"/s)"),
                       limits = c(xmin,xmax))+
    scale_y_continuous(name = "% of flow less than daily discharge",limits = c(0,1))+
    rvn_theme_RavenR()+
    #ggtitle("Flow Exceedance")+
    scale_colour_brewer(type = "qual", palette = 3)+
    rvn_theme_RavenR()

  return(p1)
}

