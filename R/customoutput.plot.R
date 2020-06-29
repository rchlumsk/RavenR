#' Plot Raven Custom Output
#'
#' customoutput.plot is used to plot the custom output from Raven
#'
#' customoutput.plot plots the custom output from a Raven model. The custom
#' output should be first read in using the custom.read function.
#'
#' @param cust custom output object from custom.read
#' @param IDs (optional) array of HRU IDs, subbasin IDs, HRU Group names/IDs to
#' include in plots
#' @param prd (optional) period to use in plotting
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{customoutput.plot}} for plotting custom output
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven plot custom output
#' @examples
#'
#'# read in custom output from sample data
#'data("custom.data")
#'
#'# plot custom data
#'mycustomdata <- custom.data
#'customoutput.plot(mycustomdata,IDs=seq(1,32),prd="2002-10-01/2003-09-01")
#'
#' @export customoutput.plot
customoutput.plot <-function(cust,IDs=NULL,prd=NULL){

  #determine IDs lis, if null - includes all
  if (is.null(IDs)){
    IDs=colnames(cust)
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
    ts=cust[,1]
    N <- nrow(ts)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(ts[1,1]),month(ts[1,1]),day(ts[1,1]),
                   year(ts[N,1]),month(ts[N,1]),day(ts[N,1]) )
  }

  old.par <-par(mfrow=c(1,1));

  rav.dt   <-xtsAttributes(cust)$ datatype;# odd space involved
  stat.agg <-xtsAttributes(cust)$ stat.agg;
  time.agg <-xtsAttributes(cust)$ time.agg
  space.agg<-xtsAttributes(cust)$ space.agg
  runname  <-xtsAttributes(cust)$ runname

  plot.title=paste(time.agg,' ',stat.agg,' ',rav.dt,' by ', space.agg)
  vaxis.title=rav.dt

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
  plottype='l'; # default for state variable
  if (rav.dt  %in% flist){
    plottype='s';
  }

  # Create a basic plot with a custom title
  # plot.title <- sprintf('Precipitation in subbasin %s',colnames(custom1)[21])
  allmin<-1e99
  allmax<--1e99
  for (ID in IDs){
   allmax=max(allmax,max(cust[prd,ID]))
   allmin=min(allmin,min(cust[prd,ID]))
  }
  ylimits=c(allmin,allmax)
  plot(lubridate::date(cust[prd,IDs[1]]),0*cust[prd,IDs[1]],xlab='Date/Time',ylab=vaxis.title,ylim=ylimits,type='s',main=plot.title)
  # plot.new() # precip in subbasin 21
  #type='h',main=plot.title

  for (ID in IDs){
    lines(lubridate::date(cust[prd,ID]),cust[prd,ID],type=plottype)
  }
  legend(x='topright',legend=IDs,lty=rep(1,length(IDs)),col=rep('black',length(IDs)))

  # if variable is state variable (not forcing), should use type='l'

  par(old.par)

  return(TRUE)
}
