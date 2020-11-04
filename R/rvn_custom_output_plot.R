#' @title Plot Raven Custom Output
#'
#' @description
#' rvn_custom_output_plot is used to plot the custom output from Raven
#'
#' @details
#' The custom output should be first read in using the rvn_custom_read function. Note that in this case the
#' plot title is included, generated from the information in the filename. This plot title may be changed with
#' ggplot2 commands.
#'
#' @param cust custom output object from custom.read
#' @param IDs (optional) array of HRU IDs, subbasin IDs, HRU Group names/IDs to
#' include in plots
#' @param prd (optional) period to use in plotting
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{rvn_custom_output_plot}} for plotting custom output
#'
#' @examples
#'
#' # read in custom output from sample data
#' ff <- system.file("extdata/run1_SNOW_Daily_Average_ByHRU.csv", package="RavenR")
#' mycustomdata <- rvn_custom_read(ff)
#'
#' # plot custom data (first 10 HRUs)
#' rvn_custom_output_plot(mycustomdata, IDs=seq(1,10), prd="2002-10-01/2003-06-01")
#'
#' @export rvn_custom_output_plot
#' @importFrom ggplot2 fortify ggplot aes geom_line ylab xlab scale_colour_brewer scale_x_datetime geom_step ggtitle
#' @importFrom reshape2 melt
#' @importFrom xts xtsAttributes
rvn_custom_output_plot <-function(cust, IDs=NULL, prd=NULL)
{

  Index <- value <- variable <- NULL

  #determine IDs lis, if null - includes all
  if (is.null(IDs)){
    IDs=colnames(cust)
  }

  # Put into approrpiate format
  df.plot <- fortify(cust[,IDs])
  df.plot <- reshape2::melt(df.plot, id.vars = "Index")

  # Break down xts attributes to get title
  xtsAttributes(cust) %>% data.frame() -> xdf
  rav.dt <- xdf$datatype
  stat.agg <-xdf$stat_agg
  time.agg <- xdf$time_agg
  # space.agg<-unlist(strsplit(xtsAttributes(cust)$ space.agg, split = "By"))[2]
  space.agg <-  strsplit(xdf$space_agg, split="By")[[1]][2]
  runname  <- xdf$runname

  plot.title=toupper(paste(time.agg,' ',stat.agg,' ',rav.dt,' by ', space.agg))

  # Yaxis
  yaxis.title=rav.dt

  flist=c('PRECIP',        'PRECIP_DAILY_AVE', 'PRECIP_5DAY',    'SNOW_FRAC',
          'RAINFALL',      'SNOWFALL',
          'TEMP_AVE',      'TEMP_DAILY_MIN',   'TEMP_DAILY_MAX', 'TEMP_DAILY_AVE',
          'TEMP_MONTH_MAX','TEMP_MONTH_MIN',   'TEMP_MONTH_AVE',
          'TEMP_AVE_UNC',  'TEMP_MIN_UNC',     'TEMP_MAX_UNC',
          'AIR_PRES',      'AIR_DENS',        'REL_HUMIDITY',
          'CLOUD_COVER',   'SW_RADIA',         'LW_RADIA','ET_RADIA','SW_RADIA_NET','SW_RADIA_UNC',
          'DAY_LENGTH',    'DAY_ANGLE',        'WIND_VEL',
          'PET,OW_PET',  'PET_MONTH_AVE',
          'SUBDAILY_CORR', 'POTENTIAL_MELT')

  # check and adjust the time period argument prd
  prd <- rvn_get_prd(cust, prd)
  firstsplit <- unlist(strsplit(prd,"/"))

  if (rav.dt  %in% flist){
    #Use step plot instead of lineplot
    p1 <- ggplot(data=   subset(df.plot, Index >= as.POSIXct(firstsplit[1]) & Index <= as.POSIXct(firstsplit[2])))+
      geom_step(aes(x=Index,y=value,color=variable))+
      rvn_theme_RavenR()+
      ylab(yaxis.title)+
      xlab("")+
      ggtitle(plot.title) +
      scale_colour_brewer(type = "qual", palette = 3)+
      rvn_theme_RavenR()

  } else { #line plot is default for state variables

    p1 <- ggplot(data= subset(df.plot, Index >= as.POSIXct(firstsplit[1]) & Index <= as.POSIXct(firstsplit[2])))+
      geom_line(aes(x=Index,y=value,color=variable))+
      rvn_theme_RavenR()+
      ylab(yaxis.title)+
      xlab("")+
      ggtitle(plot.title)+
      scale_colour_brewer(type = "qual", palette = 3)+
      rvn_theme_RavenR()
  }

 return(p1)
}
