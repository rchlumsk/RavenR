#' Plot Hydrograph
#'
#' rvn_hyd_plot creates a hydrograph plot for the supplied flow series, or
#' equivalently a stage plot for reservoir stages.
#'
#' This function creates a hydrograph plot using the supplied time series; any
#' series not supplied will not be plotted. If the precip time series is
#' supplied, the secondary y axis will be used to plot the precip time series.
#'
#' The function assumes that the supplied time series have the same length and
#' duration in time. If this is not true, then the defined period or period
#' calculated from the first available flow series will be used to determine
#' the plotting limits in time. If the data is used directly from Raven output,
#' this is not a concern. The supplied time series should be in xts format,
#' which again can be obtained directly by using the hyd.extract function.
#'
#' The winter_shading argument will add a transparent cyan shading for the
#' December 1st to March 31st period in each year that is plotted.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param inflow time series object of inflows to subbasin
#' @param precip time series object of precipitation
#' @param prd period to use in plotting
#' @param winter_shading optionally adds shading for winter months (default
#' TRUE)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{flow.spaghetti}} to create a spaghetti plot of annual
#' flow series
#'
#' \code{\link{hyd.extract}} to extract time series from Raven objects
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow hydrograph
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#' precip <- rvn_hydrograph_data$hyd$precip
#'
#' # create a nice hydrograph
#' rvn_hyd_plot(sim,obs,zero_axis=F)
#'
#' # create a hydrograph with precip as well;
#' ## range.mult=1.5 by default, leaves some overlap in plot axes
#' rvn_hyd_plot(sim,obs,precip=precip)
#'
#' # create a hydrograph with precip as well for a specific subperiod
#' prd <- "2003-10-01/2004-10-01"
#' rvn_hyd_plot(sim,obs,precip=precip,prd=prd)
#'
#' @export rvn_hyd_plot
rvn_hyd_plot <- function(sim=NULL,obs=NULL,inflow=NULL,precip=NULL,prd=NULL, winter_shading=T) {

  # select series to use as base in time determination
  if (!(is.null(sim))) {
    base <- sim
  } else if (!(is.null(obs))) {
    base <- obs
  } else if (!(is.null(inflow))) {
    base <- inflow
  } else {
    stop("Must supply at least one flow series to plot.")
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
  } else {
    # period is not supplied
    # define entire range as period
    N <- nrow(base)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(base[1,1]),month(base[1,1]),day(base[1,1]),
                   year(base[N,1]),month(base[N,1]),day(base[N,1]) )
  }
  
  #Create X axis limits from period
  x.min <- as.Date(unlist(strsplit(prd,"/"))[1])
  x.max <- as.Date(unlist(strsplit(prd,"/"))[2])
  
  #Create data frame for plotting
  df.plot <- data.frame()
  
  if (!(is.null(sim))) {
    sim_temp <- fortify(sim)
    sim_temp$ID <- "Sim"
    colnames(sim_temp) <- c("Date","Flow","ID")
    df.plot <- rbind(df.plot,sim_temp)
  } 
  if (!(is.null(obs))) {
    obs_temp <- fortify(obs)
    obs_temp$ID <- "Obs"
    colnames(obs_temp) <- c("Date","Flow","ID")
    df.plot <- rbind(df.plot,obs_temp)
  } 
  if (!(is.null(inflow))) {
    inflow_temp <- fortify(inflow)
    inflow_temp$ID <- "Inflow"
    colnames(inflow_temp) <- c("Date","Flow","ID")
    df.plot <- rbind(df.plot,inflow_temp)
  }
  
  df.plot$Date <- as.Date(as.character(df.plot$Date))
  
  p1 <- ggplot()+
    geom_line(data=df.plot, aes(x=Date,y=Flow,color=ID))+
    scale_x_date(limits = c(x.min,x.max))+
    xlab("Date")+
    ylab(expression("Flow ["*m^3*"/s]"))+
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = "bottom")
  
  #Shade Winter Months 
  if (winter_shading){
    
    winter.start <- as.Date(df.plot$Date[month(df.plot$Date) == 12 & day(df.plot$Date) == 1])
    winter.end <- as.Date(df.plot$Date[month(df.plot$Date) == 3 & day(df.plot$Date) == 31])
    
    shade <- data.frame(cbind(winter.start,winter.end))
    shade$winter.start <- as.Date(shade$winter.start)
    shade$winter.end <- as.Date(shade$winter.end)
    shade$y.start <- -Inf
    shade$y.end <- Inf
    
    p1 <- p1 + 
      geom_rect(data = shade, aes(xmin=winter.start,xmax=winter.end,ymin=y.start,ymax=y.end),color="grey",alpha=0.1)
    
  }
  
  #Add precipitation
  if (!(is.null(precip))){
    
    df.precip.plot <- fortify(precip)
    colnames(df.precip.plot)[1] <- "Date"
    df.precip.plot$ID <- "Precip"
    
    df.precip.plot$Date <- as.Date(as.character(df.precip.plot$Date))
    
    p2 <- ggplot()+
      geom_bar(data=df.precip.plot, aes(x=Date,y=precip), stat="identity", color = "blue")+
      scale_x_date(limits = c(x.min,x.max))+
      theme_bw()+
      ylab("Precip [mm]")+
      xlab("")
    
    
    p1 <- cowplot::plot_grid(p2,p1,nrow=2)
    
  }
  
  
  
  plot(p1)
  return(p1)
}

