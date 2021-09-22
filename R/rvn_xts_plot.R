#' @title Create plot from xts data
#'
#' @description
#' Generic function for plotting data from an xts format
#' using the ggplot2 function.
#'
#' @details
#' Creates a plot using the supplied time series in
#' xts format. The xts object is converted to a tibble using the \code{\link{rvn_fortify_xts}}
#' function, and all columns are plotted by their respective names.
#'
#' The winter_shading argument will add a transparent grey shading for the
#' specified period by wsdates in each year that is plotted. Note that by changing the
#' wsdates parameter values, the shading can be applied to any time of year.
#'
#' wsdates is formatted as c(winter start month, winter start day, winter end month, winter end day).
#' By default, wsdates is set to generate shading for December 1st to March 31st.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param x time series object (xts) of data to plot
#' @param prd period to use in plotting
#' @param winter_shading optionally adds shading for winter months (default \code{FALSE})
#' @param wsdates integer vector of winter shading period dates (see details)
#' @return \item{p1}{returns ggplot plot object}
#'
#' @seealso \code{\link{rvn_hyd_plot}} to create a hydrograph plot
#'
#' \code{\link{rvn_hyd_extract}} to extract time series from Raven objects
#'
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#' run1 <- rvn_hyd_read(ff)
#'
#' # create a hydrograph with the generic xts plotting function
#' rvn_xts_plot(run1$hyd[,2:5])
#'
#' # add shading for the month of August
#' rvn_xts_plot(run1$hyd[,2:5], winter_shading=TRUE, wsdates=c(8,1,8,31))
#'
#' @export rvn_xts_plot
#' @importFrom ggplot2 fortify ggplot geom_line scale_x_date xlab ylab theme aes scale_colour_brewer geom_bar
#' @importFrom cowplot plot_grid
#' @importFrom tidyr pivot_longer
rvn_xts_plot <- function(x=NULL, prd=NULL,
                         winter_shading=FALSE, wsdates=c(12,1,3,31))
{

  Date <- plot_values <- plot_type <- y.start <- y.end <- NULL

  prd <- rvn_get_prd(x[,1], prd)

  #Create data frame for plotting - including prd subset
  df.plot <- rvn_fortify_xts(x[prd])

  df.plot <- pivot_longer(df.plot,
               cols=colnames(df.plot)[-1],
               names_to="plot_type",
               values_to="plot_values")

  p1 <- ggplot()+
    geom_line(data=df.plot, aes(x=Date,y=plot_values,color=plot_type))+
    # scale_x_date(limits = c(x.min,x.max))+
    scale_x_date()+
    xlab("Date")+
    ylab(expression("Flow ("*m^3*"/s)"))+
    rvn_theme_RavenR()+
    theme(legend.position = "bottom") +
    scale_colour_brewer(type = "qual", palette = 3)

  #Shade Winter Months
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

  return(p1)
}
