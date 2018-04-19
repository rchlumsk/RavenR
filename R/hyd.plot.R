#' Plot Hydrograph
#'
#' hyd.plot creates a hydrograph plot for the supplied flow series, or
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
#' The winter.shading argument will add a transparent cyan shading for the
#' December 1st to March 31st period in each year that is plotted.
#'
#' The range.mult arguments (range.mult.hyd, range.mult.precip) will increase the maximum value that is plotted in
#' the flows and the precip values. This is useful in preventing overlap if
#' precip is also plotted (i.e. with precip as well, range.mult=1.5 works
#' well). This value should not be less than 1.0, otherwise the values will be
#' cutoff in the plot. A value of 2 in each argument will ensure there is no overlap
#' in the hydrographs and precipitation plots.
#'
#' ylabel is the label on the y axis, defined using y.lab in the plot function.
#' This defaults to 'Flow [m3/s]' intended for plotting hydrographs.
#'
#' leg.pos is the position for the legend to be placed, e.g. 'topleft',
#' 'right', etc., and is consistent with the legend function options. If this
#' is left null, the function will place it either topleft or left, depending
#' on whether precip is also plotted (i.e. left if precip added, topleft
#' otherwise).
#'
#' leg.box is a boolean for whether to put the legend in an opaque. white box
#' or not. If left as NULL, the function will automatically not use a white box
#' and leave the background of the legend transparent.
#'
#' zero.axis can be used to set the min value of the y axis (or axes if precip
#' also plotted) to zero. Note that by default, R will plot the values with a
#' slight buffer for presentation. A warning that if this option is set to
#' TRUE, the minimum value is set to zero without checking if any flow values
#' are less than zero. This option should not be used for stage plotting, since
#' most reservoirs are not in the range of 'zero' stage for normal operations,
#' since stage is reported to elevation and not to stage bottom, typically.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param inflow time series object of inflows to subbasin
#' @param precip time series object of precipitation
#' @param prd period to use in plotting
#' @param winter.shading optionally adds shading for winter months (default
#' TRUE)
#' @param range.mult.hyd range multiplier for max value in hydrograph (default 1.5)
#' @param range.mult.precip range multiplier for max value in precipitation plot (default 1.5)
#' @param ylabel text label for y-axis of the plot (default 'Flow [m3/s]')
#' @param leg.pos string specifying legend placement on plot
#' @param leg.box boolean on whether to put legend in an opaque box
#' @param zero.axis fixes the y axis to start exactly at zero (default TRUE)
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
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#' precip <- hydrograph.data$hyd$precip
#'
#' # create a nice hydrograph
#' hyd.plot(sim,obs,zero.axis=F)
#'
#' # create a hydrograph with precip as well;
#' ## range.mult=1.5 by default, leaves some overlap in plot axes
#' hyd.plot(sim,obs,precip=precip)
#'
#' # hydrograph with no overlap, range.mult = 2
#' hyd.plot(sim,obs,range.mult.hyd=2,range.mult.precip=2,precip=precip)
#'
#' # create a hydrograph with precip as well for a specific subperiod
#' prd <- "2003-10-01/2004-10-01"
#' hyd.plot(sim,obs,precip=precip,prd=prd)
#'
#' @export hyd.plot
hyd.plot <- function(sim=NULL,obs=NULL,inflow=NULL,precip=NULL,prd=NULL,
              winter.shading=T,range.mult.hyd=1.5,range.mult.precip=1.5,ylabel="Flow [m3/s]",leg.pos=NULL,leg.box=NULL,zero.axis=T) {

  # check range.mult input
  if (!(is.na(range.mult.hyd))) {
    if (range.mult.hyd <= 0) {
      stop("range.mult.hyd must be a positive value.")
    }
    if (range.mult.hyd < 1) {
      warning("range.mult.hyd is less than one, plot may be cut off.")
    }
  }
  if (!(is.na(range.mult.precip))) {
    if (range.mult.precip <= 0) {
      stop("range.mult.precip must be a positive value.")
    }
    if (range.mult.precip < 1) {
      warning("range.mult.precip is less than one, plot may be cut off.")
    }
  }

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
    # add conversion to date with xts format check ?

  } else {
    # period is not supplied

    # define entire range as period
    N <- nrow(base)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(base[1,1]),month(base[1,1]),day(base[1,1]),
                   year(base[N,1]),month(base[N,1]),day(base[N,1]) )
  }

  ##### ------

  # capture plotting parameters, restore afterwards
  .pardefault <- par(no.readonly = T)

  # set parameters for plotting; then plot
  if(!(is.null(precip))) {
    par(mar=c(5, 4, 4, 4) + 0.1)
  }
  if (zero.axis) {
    # sets the interval calculation in plotting to be right to specified limits
    # otherwise extends by 4% by default
    par(yaxs='i')
  }

  y.max <- max(c(sim[prd],obs[prd],inflow[prd]),na.rm=T)*range.mult.hyd
  if (zero.axis) {
    y.min<-0
  } else {
    y.min <- min(c(sim[prd],obs[prd],inflow[prd]),na.rm=T)
  }

  plot(lubridate::date(base[prd]),base[prd],xlab="Date",ylab="Flow [m3/s]",
       col='white',type='l',ylim=c(y.min,y.max), panel.first=grid())
  if (winter.shading) {
    # shaded winter months
    temp <- base[((month(base[,1]) == 12) & (day(base[,1]) == 1)) | ((month(base[,1]) == 3) & (day(base[,1]) == 31))]
    ep <- match(lubridate::date(temp),lubridate::date(base))
    if (month(base[ep[1]])==3) {
      ep <- ep[-1]
    }
    if (month(base[ep[length(ep)]])==12) {
      ep <- ep[-length(ep)]
    }
    bc <- col.transparent('cyan',50)
    for (k in seq(1,length(ep),2)) {
      cord.x <- c(lubridate::date(base[ep[k]]),lubridate::date(base[ep[k]]),lubridate::date(base[ep[k+1]]),lubridate::date(base[ep[k+1]]))
      cord.y <- c(-1e3,max(base,obs,na.rm=T)*1e3,max(base,obs,na.rm=T)*1e3,-1e3)
      polygon(cord.x,cord.y,col=bc,border=NA)
    }
  }

  # define legend items
  leg.items <- c()
  leg.cols <- c()
  leg.lty <- c()
  leg.lwd <- c()

  if (!(is.null(sim))) {
    lines(lubridate::date(sim),sim,col="red",lty=5)
    leg.items <- c(leg.items,'sim')
    leg.cols <- c(leg.cols,'red')
    leg.lty <- c(leg.lty,5)
    leg.lwd <- c(leg.lwd,1)
  }
  if (!(is.null(obs))) {
    lines(lubridate::date(obs),obs,col='black')
    leg.items <- c(leg.items,'obs')
    leg.cols <- c(leg.cols,'black')
    leg.lty <- c(leg.lty,1)
    leg.lwd <- c(leg.lwd,1)
  }
  if (!(is.null(inflow))) {
    lines(lubridate::date(sim),sim,col="green",lty=3)
    leg.items <- c(leg.items,'inflow')
    leg.cols <- c(leg.cols,'green')
    leg.lty <- c(leg.lty,3)
    leg.lwd <- c(leg.lwd,1)
  }
  if (!(is.null(precip))) {
    par(new=T)
    precip.col <- col.transparent('blue',100)
    plot(lubridate::date(precip[prd]),precip[prd],col=precip.col,lty=1,lwd=1,
         type='h',ylim=rev(c(0,max(precip,na.rm=T)*range.mult.precip)),xaxt='n',yaxt='n',
         xlab="",ylab="")
    axis(4)
    mtext("Precipitation [mm]",side=4,line=2.5)

    leg.items <- c(leg.items,'precip')
    leg.cols <- c(leg.cols,precip.col)
    leg.lty <- c(leg.lty,1)
    leg.lwd <- c(leg.lwd,1)
  }

  if (is.null(leg.pos)) {
    if (!(is.null(precip))) {
      leg.pos <- 'left'
    } else {
      leg.pos <- 'topleft'
    }
  }
  if (is.null(leg.box)) {
    leg.box <- 'n'
  } else {
    if (leg.box) {
      leg.box <- 'o'
    } else {
      leg.box <- 'n'
    }
  }

  # add legend to plot
  legend(x=leg.pos,legend=leg.items,lty=leg.lty,col=leg.cols,
         lwd=leg.lwd,bty=leg.box,cex=0.8,inset=0.01)

  # restore plotting parameters
  par(.pardefault)

  return(TRUE)
}

