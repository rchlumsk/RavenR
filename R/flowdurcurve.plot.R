#' Plots summary of watershed forcing functions
#'
#' flowdurcurve.plot generation a flow duration curve plot.
#'
#' This function creates a flow duration curve using the hyd.extract obejct for
#' a given basin. The hydrograph object passed should be the output from the
#' hyd.extract function, which has attributes for sim and obs; if the obs is
#' NULL, only the sim FDC will be plotted.
#'
#' If the seasonal argument is included, the winter and summer FDC lines will
#' be included on the plot as well.
#'
#' @param sim simulated hydrograph xts time series
#' @param obs (optional) observed hydrograph xts time series
#' @param prd (optional) time period over which the plot is generated
#' @param seasonal (optional) add the winter and summer FDC
#' @seealso \code{\link{hyd.read}} for reading in the Hydrographs.csv file
#' \code{\link{hyd.extract}} for extracting basin flow information from a
#' hyd.read object
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow duration curve plot diagnostics
#' @examples
#'
#' # load sample hydrograph data, two years worth of sim/obs
#' data(hydrograph.data)
#' sim <- hydrograph.data$hyd$Sub36
#' obs <- hydrograph.data$hyd$Sub36_obs
#'
#' # create FDC plot, sim only
#' flowdurcurve.plot(sim)
#'
#'  # create seasonal FDC plot with sim and obs data
#' flowdurcurve.plot(sim,obs,seasonal=T)
#'
#' @export flowdurcurve.plot
flowdurcurve.plot <-function(sim=NULL,obs=NULL,prd=NULL,seasonal='F'){

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
    #print(sim.month)
    summer<-sim[sim.month >=5 & sim.month<9] # May to September
    winter<-sim[sim.month <5 | sim.month>=9]
  }

  if (!(is.null(obs))) {
    obs=sim+rep(10,length(sim)); # temp debug
  }

  # require(png)
  # require(grid)
  xmax=10^(ceiling(log10(max(sim))));
  xmin=10^(floor(log10(min(sim))));
  plot(ecdf(as.numeric(sim)), log='x',xlim=c(xmin,xmax),
       ylim=c(0,1),col='Blue',xlab='Q (m3/s)', ylab='% of flows less than Q',main='Flow Exceedance')

  # define legend items (see code from hyd.plot)
  leg.items <- c('sim')
  leg.cols <- c('blue')
  leg.lty <- c(1)

  if (!is.null(obs)){
    lines(ecdf(as.numeric(obs)),col='black')
    #legend(x='bottomright',legend=c('sim','obs'),lty=c(1,1),col=c('blue','black'))
    leg.items <- c(leg.items,'obs')
    leg.cols <- c(leg.cols,'black')
    leg.lty <- c(leg.lty,1)
  }
  if (seasonal!='F'){
    lines(ecdf(as.numeric(summer)),col='red',do.p=FALSE)
    lines(ecdf(as.numeric(winter)),col='cadetblue2')
    # legend(x='bottomright',legend=c('sim','obs','Qsummer','Qwinter'),lty=c(1,1,1,1),col=c('blue','black','red','cadetblue2'))
    leg.items <- c(leg.items,'Qsummer','Qwinter')
    leg.cols <- c(leg.cols,'red','cadetblue2')
    leg.lty <- c(leg.lty,1,1)
  }

  legend(x='bottomright',legend=leg.items,lty=leg.lty,col=leg.cols,inset=0.01,cex=0.9)

  #icon.draw <- function(image, x, y, size) {
  #  logo <- rasterGrob(image = image,
  #                     x = unit(x, "npc"), y = unit(y, "npc"), height = unit(size, "cm"))
  #  grid.draw(logo)
  #}
  # plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
  #img<-png::readPNG("RavenIcon.png")
  # plot(1:10,1:10)
  #icon.draw(img,0.9,0.7,1)
}

