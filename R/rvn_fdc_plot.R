#' Plots summary of watershed forcing functions
#'
#' rvn_fdc_plot generation a flow duration curve plot.
#'
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
#' @param seasonal (optional) add the winter and summer FDC
#' @seealso \code{\link{rvn_hyd_read}} for reading in the Hydrographs.csv file
#' \code{\link{rvn_hyd_extract}} for extracting basin flow information from a
#' rvn_hyd_read object
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow duration curve plot diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # create FDC plot, sim only
#' rvn_fdc_plot(sim)
#'
#'  # create seasonal FDC plot with sim and obs data
#' rvn_fdc_plot(sim,obs,seasonal=T)
#'
#' @export rvn_fdc_plot
rvn_fdc_plot <-function(sim=NULL,obs=NULL,prd=NULL,seasonal='F'){

  if (is.null(sim)) {
    stop("sim is required for plotting.")
  }

  # determine period ----
  # determine the period to use
  if (!(is.null(prd))) {

    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
    # add conversion to date with xts format check ?
  }
  else
  {
    # period is not supplied, define entire range as period
    N <- nrow(sim)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(sim[1]),month(sim[1]),day(sim[1]),
                   year(sim[N]),month(sim[N]),day(sim[N]) )
  }

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
  
  if (seasonal != "F"){
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
    scale_x_continuous(trans = 'log10', name = expression("Q ["*m^3*"/s]"),limits = c(xmin,xmax))+
    scale_y_continuous(name = "% of flow less than Q",limits = c(0,1))+
    theme_bw()+
    ggtitle("Flow Exceedance")+
    theme(plot.title = element_text(hjust=0.5))
  
  return(p1)
}

