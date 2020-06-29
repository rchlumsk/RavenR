#' Flow Spaghetti Plot
#'
#' flow.spaghetti creates a spaghetti plot of the flow series provided.
#'
#' This function creates a spaghetti plot of the annual flow series in each
#' year of data provided. The flows are plotted for each water year of data
#' available, set as October 1st.
#'
#' Note that the plotting to the day of year is approximate in order to
#' simplify the plotting of leap years and non-leap years. The years are
#' plotted including day 366 and starting on day 274, regardless of whether it
#' is a leap year or not. This is likely without consequence in seeing the
#' trends between water years, however the user is warned of this deficiency.
#'
#' The include.legend argument will add a legend to the right-hand side of the
#' plot, if true. The legend will be labelled as the year for each flow series,
#' where the year is the date-ending year (i.e. for the 2003-10-01 to
#' 2004-09-30 water year, the flow series will be labelled as 2004). This
#' argument will also cause the plot margins to be adjusted so that the legend
#' can be included. Note that the current setup to accomodate approximately 15
#' years of flow series in the legend, beyond that the legend may not be
#' aesthetically pleasing.
#'
#' The colour.wheel argument, if TRUE, will plot each water year in a different
#' randomly sampled colour. If FALSE, all years will be plotted in black. If
#' colour.wheel is FALSE, the legend is recommended to be set to FALSE as well.
#'
#' The flow series provided should be of time series (xts) format.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles.
#'
#' @param flow time series object of simulated flows
#' @param include.legend option to include a legend (default TRUE)
#' @param colour.wheel option to use a different colour for each year plotted
#' (default TRUE)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{flow.scatterplot}} to create a scatterplot of flow
#' values
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven flow spaghetti diagnostics
#' @examples
#'
#'# load sample hydrograph data, two years worth of sim/obs
#'data(hydrograph.data)
#'sim <- hydrograph.data$hyd$Sub36
#'sim2 <- hydrograph.data$hyd$Sub43
#'
#'# create spaghetti plot of simulated flows
#'flow.spaghetti(sim)
#'
#'# create spaghetti plot without legend or different colours
#'flow.spaghetti(sim2,include.legend=F,colour.wheel=F)
#'
#' @export flow.spaghetti
flow.spaghetti <- function(flow,include.legend=T,colour.wheel=T) {

  # initial plot setup
  ticks.at <- seq(1,366,1)
  ticks.seq <- c(seq(274,366,1),seq(1,273,1))
  ind.start <- seq(1,366,31)
  nyears <- round(nrow(flow)/365)
  ep <- wyear.indices(flow)
  y.min <- min(flow)
  y.max<-max(flow)

  # define colours to use
  # eventually want to update this to get it to pick 'nice' colours consistently
  if (colour.wheel) {
    cw <- sample(colours(), nyears,replace=F)
  } else {
    cw <- rep('black',nyears)
  }

  ## start plot
  if (include.legend) {
    .pardefault <- par(no.readonly=T)
    par(mar=c(5,4,4,4)+0.1)
    par(xpd=T)
  }
  plot(ticks.at,rnorm(366),xaxt='n',ylim=c(y.min,y.max),col='white',
       ylab='Flow [m3/s]',xlab='Day of Year')
  axis(1, at=ticks.at[ind.start],labels=ticks.seq[ind.start],cex.axis=0.7)

  j = 1
  for (i in 1:(length(ep)-1)) {
    temp <- coredata(flow[ep[i]:ep[i+1],])
    temp <- temp[1:(length(temp)-1)]
    lines(seq(1,length(temp),1),temp,lty=5,col=cw[j])
    j = j+1
  }

  # add legend
  if (include.legend) {
    labs <- matrix(ncol=1,nrow=j-1)

    # re-obtain periods and year labels; don't need for starting point
    labs <- lubridate::year(flow[ep])[-1]
    legend(x=380,y=y.max,legend=labs,bty='n',col=cw,lty=rep(5,length(labs)),cex=0.8)
    par(.pardefault)
  }
  return(TRUE)
}

