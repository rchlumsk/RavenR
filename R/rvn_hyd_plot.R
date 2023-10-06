#' @title Create Hydrograph Plot
#'
#' @description
#' rvn_hyd_plot creates a hydrograph plot object for the supplied flow series, or
#' equivalently a stage plot for reservoir stages.
#'
#' @details
#' Creates a hydrograph plot using the supplied time series; any
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
#' The winter_shading argument will add a transparent grey shading for the
#' specified period by wsdates in each year that is plotted.
#'
#' wsdates is formatted as c(winter start month, winter start day, winter end month, winter end day).
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param inflow time series object of inflows to subbasin
#' @param precip time series object of precipitation
#' @param prd period to use in plotting
#' @param winter_shading optionally adds shading for winter months (default \code{FALSE})
#' @param wsdates integer vector of winter shading period dates (see details)
#' @return \item{p1}{returns ggplot plot object}
#'
#' @seealso \code{\link{rvn_flow_spaghetti}} to create a spaghetti plot of annual
#' flow series
#'
#' \code{\link{rvn_hyd_extract}} to extract time series from Raven objects
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#' run1 <- rvn_hyd_read(ff)
#' sim <- run1$hyd$Sub36
#' obs <- run1$hyd$Sub36_obs
#' precip <- run1$hyd$precip
#'
#' # create a nice hydrograph
#' rvn_hyd_plot(sim,obs)
#'
#' # create a hydrograph with precip as well;
#' rvn_hyd_plot(sim,obs,precip=precip)
#'
#' # create a hydrograph with precip as well for a specific subperiod
#' prd <- "2003-10-01/2004-10-01"
#' rvn_hyd_plot(sim,obs,precip=precip,prd=prd)
#'
#' # add the winter shading
#' rvn_hyd_plot(sim,obs,precip=precip,prd=prd, winter_shading=TRUE)
#'
#' # change winter shading dates (Nov 1st to April 15th)
#' rvn_hyd_plot(sim,obs,precip=precip,prd=prd, winter_shading=TRUE, wsdates=c(11,1,4,15))
#'
#' @export rvn_hyd_plot
#' @importFrom ggplot2 fortify ggplot geom_line xlab ylab theme aes scale_colour_brewer geom_bar
#' @importFrom cowplot plot_grid
rvn_hyd_plot <- function(sim=NULL,obs=NULL,inflow=NULL,precip=NULL,prd=NULL,
                         winter_shading=FALSE, wsdates=c(12,1,3,31))
{

  Date <- Flow <- ID <- y.start <- y.end <- NULL

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

  prd <- rvn_get_prd(sim, prd)

  #Create X axis limits from period
  # x.min <- as.Date(unlist(strsplit(prd,"/"))[1])
  # x.max <- as.Date(unlist(strsplit(prd,"/"))[2])


  #Create data frame for plotting
  df.plot <- data.frame()

  if (!(is.null(sim))) {
    sim <- sim[prd]
    sim_temp <- fortify(sim)
    sim_temp$ID <- "Sim"
    colnames(sim_temp) <- c("Date","Flow","ID")
    df.plot <- rbind(df.plot,sim_temp)
  }
  if (!(is.null(obs))) {
    obs <- obs[prd]
    obs_temp <- fortify(obs)
    obs_temp$ID <- "Obs"
    colnames(obs_temp) <- c("Date","Flow","ID")
    df.plot <- rbind(df.plot,obs_temp)
  }
  if (!(is.null(inflow))) {
    inflow <- inflow[prd]
    inflow_temp <- fortify(inflow)
    inflow_temp$ID <- "Inflow"
    colnames(inflow_temp) <- c("Date","Flow","ID")
    df.plot <- rbind(df.plot,inflow_temp)
  }

  df.plot$Date <- as.Date(as.character(df.plot$Date))

  p1 <- ggplot()+
    geom_line(data=df.plot, aes(x=Date,y=Flow,color=ID))+
    # scale_x_date(limits = c(x.min,x.max))+
    scale_x_date()+
    xlab("Date")+
    ylab(expression("Flow ("*m^3*"/s)"))+
    rvn_theme_RavenR()+
    theme(legend.position = "bottom") +
    # scale_colour_brewer(type = "qual", palette = 3)+
    scale_colour_manual(name='Legend',values=c("Obs"='black',"Sim"='darkorange',"Inflow"="purple"))

  #Shade Winter Months
  if (winter_shading){

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

  #Add precipitation
  if (!(is.null(precip))){

    df.precip.plot <- rvn_fortify_xts(precip[prd])

    if ("precip" %notin% colnames(precip)) {
      warning("'precip' column name not found in precip object, using first column as precipitation data anyway.")

      if (ncol(df.precip.plot) == 2) {
        colnames(df.precip.plot) <- c("Date","precip")
      } else {
        colnames(df.precip.plot) <- c(c("Date","precip"), colnames(df.precip.plot)[3:ncol(df.precip.plot)])
      }
    }

    p2 <- ggplot()+
      geom_bar(data=df.precip.plot, aes(x=Date,y=precip), stat="identity", color = "blue")+
      # scale_x_date(limits = c(x.min,x.max))+
      scale_x_date()+
      ylab("Precip (mm/d)")+
      xlab("")+
      rvn_theme_RavenR()

    p1 <- plot_grid(p2,p1,nrow=2, rel_heights=c(1,1.15), align="v")
  }

  return(p1)
}
