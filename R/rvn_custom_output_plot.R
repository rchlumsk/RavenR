#' Plot Raven Custom Output
#'
#' rvn_custom_output_plot is used to plot the custom output from Raven
#'
#' rvn_custom_output_plot plots the custom output from a Raven model. The custom
#' output should be first read in using the custom.read function.
#'
#' @param cust custom output object from custom.read
#' @param IDs (optional) array of HRU IDs, subbasin IDs, HRU Group names/IDs to
#' include in plots
#' @param prd (optional) period to use in plotting
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{rvn_custom_output_plot}} for plotting custom output
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven plot custom output
#' @examples
#'
#' # read in custom output from sample data
#' ff <- system.file("extdata/run1_SNOW_Daily_Average_ByHRU.csv", package="RavenR")
#' mycustomdata <- rvn_custom_read(ff)
#'
#' # plot custom data (first 10 HRUs)
#' rvn_custom_output_plot(mycustomdata, IDs=seq(1,10), prd="2002-10-01/2003-09-01")
#'
#' @export rvn_custom_output_plot
rvn_custom_output_plot <-function(cust, IDs=NULL, prd=NULL){

  #determine IDs lis, if null - includes all
  if (is.null(IDs)){
    IDs=colnames(cust)
  }

  # Put into approrpiate format
  df.plot <- fortify(cust[,IDs])
  df.plot <- reshape2::melt(df.plot, id.vars = "Index")

  # Break down xts attributes to get title
  rav.dt   <-xtsAttributes(cust)$ datatype;# odd space involved
  stat.agg <-xtsAttributes(cust)$ stat.agg;
  time.agg <-xtsAttributes(cust)$ time.agg
  space.agg<-unlist(strsplit(xtsAttributes(cust)$ space.agg, split = "By"))[2]
  runname  <-xtsAttributes(cust)$ runname

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

  if (rav.dt  %in% flist){
    #Use step plot instead of lineplot
    p1 <- ggplot()+
      geom_step(data = df.plot, aes(x=Index,y=value,color=variable))+
      rvn_theme_RavenR()+
      ylab(yaxis.title)+
      xlab("")+
      #ggtitle(plot.title) +
      scale_colour_brewer(type = "qual", palette = 3)


  } else { #line plot is default for state variables

    p1 <- ggplot()+
      geom_line(data = df.plot, aes(x=Index,y=value,color=variable))+
      rvn_theme_RavenR()+
      ylab(yaxis.title)+
      xlab("")+
      #ggtitle(plot.title)+
      scale_colour_brewer(type = "qual", palette = 3)

  }

  # Change plot limits to period
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

    #Limit plot to period
    p1 <- p1 +
      scale_x_datetime(limits=c(as.POSIXct(firstsplit[1]),as.POSIXct(firstsplit[2])))
  }

 plot(p1)

 return(p1)
}
