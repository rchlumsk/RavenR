#' Plots summary of watershed forcing functions
#'
#' forcings.plot generates a set of 5 plots (precip,temperature,PET,radiation,
#' and potential melt), which summarize the watershed-averaged forcings.
#'
#' This function creates multiple plots from a ForcingFunctions.csv file
#' structure generating using RavenR's forcings.read function
#'
#' @param forcings forcings attribute from forcings.read function
#' @param prd (optional) time period over which the plots are generated
#' @seealso \code{\link{forcings.read}} for the function used to read in the
#' forcings function data
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven forcing plot diagnostics
#' @examples
#'
#' # read in sample forcings data
#' data("forcing.data")
#' fdata <- forcing.data$forcings
#'
#' # plot forcings data
#' forcings.plot(fdata)
#'
#' plot subset of forcing data for 2002-2003 water year
#' prd = "2002-10-01/2003-09-30"
#' forcings.plot(fdata,prd)
#'
#' @export forcings.plot
forcings.plot <-function(forcings,prd=NULL){

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
    ts=forcings$PET
    N <- nrow(ts)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(ts[1,1]),month(ts[1,1]),day(ts[1,1]),
                   year(ts[N,1]),month(ts[N,1]),day(ts[N,1]) )
  }

  old.par <-par(mfrow=c(5,1), oma=c(2,2,2,2), mar=c(2,4,2.5,0));

  # Precipitation
  plot(lubridate::date(forcings$rain[prd]),forcings$rain[prd]+forcings$snow[prd],type='S',xlab="",ylim=c(0,max(forcings$rain[prd]+forcings$snow[prd])),ylab='Precipitation (mm/d)',col='blue',panel.first=grid())
  lines(lubridate::date(forcings$rain[prd]),forcings$snow[prd],type='S',col='cadetblue2')

  # under construction - shade snow (not accepting date argument)
  # xx<-c(date(forcings$snow[prd]),rev(date(forcings$snow[prd])))
  # yy<-c(rep(0,nrow(forcings$snow[prd])),forcings$snow[prd])
  # polygon(xx,yy,'cadetblue2')

  lines(lubridate::date(forcings$rain[prd]),0.0*forcings$rain[prd],type='S',col='Black')

  legend(x='topright',legend=c('total precip','as snow'),lty=c(1,1),col=c('blue','cadetblue2'))
  ts=forcings$snow[prd]
  N <- nrow(ts)
  titl <- sprintf("Watershed-averaged Forcings (%d-%02d-%02d to %i-%02d-%02d)",year(ts[1,1]),month(ts[1,1]),day(ts[1,1]),year(ts[N,1]),month(ts[N,1]),day(ts[N,1]) )
  title(titl)

  # Temperature
  ylimits<-c(min(forcings$temp_daily_min[prd]),max(forcings$temp_daily_max[prd]))
  label = expression(paste("Min/Max Daily Temperature (",degree,"C)"))
  plot(lubridate::date(forcings$temp_daily_min[prd]),forcings$temp_daily_min[prd],type='S',xlab="",ylim=ylimits,ylab=label,col='red3',panel.first=grid())
  lines(lubridate::date(forcings$temp_daily_max[prd]),forcings$temp_daily_max[prd],type='S',col='red3')
  lines(lubridate::date(forcings$temp_daily_max[prd]),pmin(forcings$temp_daily_max[prd],0),type='S',col='darkorchid')
  lines(lubridate::date(forcings$temp_daily_max[prd]),pmin(forcings$temp_daily_min[prd],0),type='S',col='darkorchid')
  lines(lubridate::date(forcings$temp_daily_max[prd]),0.0*forcings$temp_daily_max[prd],type='S',col='Black')

  # PET
  plot(lubridate::date(forcings$PET[prd]),forcings$PET[prd],ylim=c(0,max(forcings$PET[prd])),type='S',xlab="Date",ylab='PET (mm/d)', col='blue4', panel.first=grid())

  # Radiation
  ylims=c(min(forcings$ET.radiation,forcings$LW.radiation,forcings$SW.radiation), max(forcings$ET.radiation,forcings$LW.radiation,forcings$SW.radiation))
  plot(lubridate::date(forcings$ET.radiation[prd]),forcings$ET.radiation[prd],ylim=ylims,type='S',xlab="Date",ylab='Radiation (MJ/m2/d)', col='blue4', panel.first=grid())
  lines(lubridate::date(forcings$SW.radiation[prd]),forcings$SW.radiation[prd],type='S', col='blue4')
  lines(lubridate::date(forcings$ET.radiation[prd]),forcings$LW.radiation[prd],type='S',col='blue4')
  lines(lubridate::date(forcings$SW.radiation[prd]),forcings$SW.radiation[prd]+forcings$LW.radiation[prd],type='S',col='black')

  # Potential melt
  plot(lubridate::date(forcings$potential.melt[prd]),forcings$potential.melt[prd],ylim=c(0,max(forcings$potential.melt[prd])),type='S',xlab="Date",ylab='Potential Melt (mm/d)',col='blue4', panel.first=grid())

  par(old.par)
}
