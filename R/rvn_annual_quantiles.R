#' Calculates Yearly Median, Upper and Lower Quantiles of Flow
#'
#' @param hgdata Time series object of observed or simulated flows
#' @param minyear Integer of minimum year (optional)
#' @param maxyear Integer of maximum year (optional)
#' @param Qlower Decimal percentage of lower quantile value (default 0.1)
#' @param Qupper Decimal percentage of upper quantile value (default 0.9)#'
#' @return \item{qdat}{Time series object of monthly median and quantile values}
#'
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#' @examples
#' system.file("extdata","run1_Hydrographs.csv", package="RavenR") %>%
#' rvn_hyd_read(.) %>%
#' rvn_hyd_extract(subs="Sub36",.) ->
#' hyd_data
#'
#' # Pull out a specific hydrograph
#' hgdata <- rvn_hydrograph_data$hyd$Sub36
#'
#' # Calculate quantiles
#' qdat <- rvn_annual_quantiles(hgdata)
#' head(qdat)
#'
#' @keywords quantile hydrograph flow statistics diagnostic
#' @export rvn_annual_quantiles
rvn_annual_quantiles <- function(hgdata, minyear=NULL, maxyear=NULL,
                           Qlower=0.1, Qupper=0.9, water_year=T) {

  #-- Assuming hgdata is a daily xts object
  # TODO: Add support for other frequencies

  #-- Handle if no min/max passed
  if (is.null(minyear)) { minyear <- year(start(hgdata))}
  if (is.null(maxyear)) { maxyear <- year(end(hgdata))}

  #-- Subset by date
  hgdata <- hgdata[paste0(toString(minyear),"/",toString(maxyear))]

  #-- Aggregate to Quantiles by Day
  # (Uses maxyear as placeholder for year)
  monthday <- as.Date(paste0(toString(maxyear), "-", month(hgdata), "-", day(hgdata)))
  if (water_year) {
    # Year is arbitrary, so subtract a year post-Oct to create water years
    #SAG comment: is this right? shouldn't you be adding a year post October?
    monthday[month(monthday) > 9] = monthday[month(monthday) > 9] - years(1)
  }
  qdat <- xts(aggregate(hgdata, by=monthday, quantile,
                        probs=c(Qlower, .5, Qupper), na.rm=T))

  return(qdat)
}
