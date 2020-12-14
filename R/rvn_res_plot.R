#' @title Plot Reservoir Stage
#'
#' @description
#' rvn_res_plot creates a reservoir stage plot for the supplied flow series, or
#' equivalently a stage plot for reservoir stages.
#'
#' @details
#' This function creates a reservoir stage plot using the supplied time series;
#' any series not supplied will not be plotted. If the precip time series is
#' supplied, the secondary y axis will be used to plot the precip time series.
#'
#' The function assumes that the supplied time series have the same length and
#' duration in time. If this is not true, then the defined period or period
#' calculated from the first available flow series will be used to determine
#' the plotting limits in time. If the data is used directly from Raven output,
#' this is not a concern. The supplied time series should be in xts format,
#' which again can be obtained directly by using the rvn_res_extract function.
#'
#' The winter_shading argument will add a transparent cyan shading for the
#' December 1st to March 31st period in each year that is plotted (or other
#' period specified by wsdates).
#'
#' wsdates is formatted as c(winter start month, winter start day, winter end month, winter end day).
#'
#' @param sim time series object of simulated stage
#' @param obs time series object of observed stage
#' @param inflow time series object of inflow to subbasin
#' @param precip time series object of precipitation
#' @param prd period to use in plotting
#' @param winter_shading optionally adds shading for winter months (default FALSE)
#' @param wsdates integer vector of winter shading period dates (see details)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{rvn_hyd_read}} for reading in the Hydrographs.csv file, and
#' \code{\link{rvn_res_extract}} to extract time series from Raven objects
#'
#' @examples
#'
#' # read in sample reservoir file
#' ff <- system.file("extdata","ReservoirStages.csv", package="RavenR")
#' rvn_res_read(ff) %>%
#' rvn_res_extract(subs="sub36", res=.) -> mystage
#' sim <- mystage$sim
#' obs <- mystage$obs
#' precip <- rvn_res_read(ff)$res$precip
#'
#' # create a nice reservoir stage plot
#' rvn_res_plot(sim,obs)
#'
#' # create a reservoir stage plot with precip as well
#' rvn_res_plot(sim,obs,precip=precip)
#'
#' # create a reservoir stage plot with precip as well for a specific subperiod
#' prd <- "2003-10-01/2005-10-01"
#' rvn_res_plot(sim,obs,precip=precip,prd=prd)
#'
#' # add winter shading
#' rvn_res_plot(sim,obs,precip=precip, winter_shading=TRUE)
#'
#' @export rvn_res_plot
#' @importFrom ggplot2 fortify ggplot geom_line scale_x_date xlab ylab theme aes scale_colour_brewer geom_bar
#' @importFrom cowplot plot_grid
rvn_res_plot <- function(sim=NULL,obs=NULL,inflow=NULL,precip=NULL,prd=NULL,
              winter_shading=FALSE,  wsdates=c(12,1,3,31))
{
  # select series to use as base in time determination
  if (!(is.null(sim))) {
    base <- sim
  } else if (!(is.null(obs))) {
    base <- obs
  } else if (!(is.null(inflow))) {
    base <- inflow
  } else {
    stop("Must supply at least one Stage series to plot.")
  }

  Date <- Stage <- ID <- y.start <- y.end <- NULL

  prd <- rvn_get_prd(sim,prd)

  #Set X axis Limits based on period
  x.min <- as.Date(unlist(strsplit(prd,"/"))[1])
  x.max <- as.Date(unlist(strsplit(prd,"/"))[2])

  #Create data frame for plotting
  df.plot <- data.frame()

  if (!(is.null(sim))) {
    sim_temp <- fortify(sim)
    sim_temp$ID <- "Sim"
    colnames(sim_temp) <- c("Date","Stage","ID")
    df.plot <- rbind(df.plot,sim_temp)
  }
  if (!(is.null(obs))) {
    obs_temp <- fortify(obs)
    obs_temp$ID <- "Obs"
    colnames(obs_temp) <- c("Date","Stage","ID")
    df.plot <- rbind(df.plot,obs_temp)
  }

  if (!(is.null(inflow))) {
    inStage_temp <- fortify(inflow)
    inStage_temp$ID <- "Inflow"
    colnames(inStage_temp) <- c("Date","Stage","ID")
    df.plot <- rbind(df.plot,inStage_temp)
  }

  df.plot$Date <- as.Date(as.character(df.plot$Date))

  p1 <- ggplot()+
    geom_line(data=df.plot, aes(x=Date,y=Stage,color=ID))+
    scale_x_date(limits = c(x.min,x.max))+
    xlab("Date")+
    ylab("Stage (m)")+
    rvn_theme_RavenR()+
    theme(legend.position = "bottom") +
    scale_colour_brewer(type = "qual", palette = 3)

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

    df.precip.plot <- fortify(precip)
    colnames(df.precip.plot)[1] <- "Date"
    df.precip.plot$ID <- "Precip"

    df.precip.plot$Date <- as.Date(as.character(df.precip.plot$Date))

    p2 <- ggplot()+
      geom_bar(data=df.precip.plot, aes(x=Date,y=precip), stat="identity", color = "blue")+
      scale_x_date(limits = c(x.min,x.max))+
      ylab("Precip (mm)")+
      xlab("")+
      rvn_theme_RavenR()

    p1 <- plot_grid(p2,p1,nrow=2)
  }

  return(p1)
}

