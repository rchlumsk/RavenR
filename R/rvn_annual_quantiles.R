#' @title Calculates Yearly Median, Upper and Lower Quantiles of Flow
#'
#' @description
#' Calculate the quantiles for each day of the year based on the supplied time series.
#'
#' @param hgdata Time series object of observed or simulated flows
#' @param prd time period for subset in character format "YYYY-MM-DD/YYYY-MM-DD"
#' @param Qlower Decimal percentage of lower quantile value (default 0.1)
#' @param Qupper Decimal percentage of upper quantile value (default 0.9)
#' @param water_year booolean on whether to sort quantiles by water year start date (default TRUE)
#' @param mm month of water year ending (default 9)
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
#' hgdata <- hyd_data$sim
#'
#' # Calculate quantiles
#' qdat <- rvn_annual_quantiles(hgdata)
#' head(qdat)
#'
#' @export rvn_annual_quantiles
#' @importFrom stats end aggregate quantile
#' @importFrom lubridate month day year date years
#' @importFrom xts xts
rvn_annual_quantiles <- function(hgdata, prd=NULL,
                           Qlower=0.1, Qupper=0.9,
                           water_year=TRUE, mm=9)
{

  #-- Assuming hgdata is a daily xts object
  # TODO: Add support for other frequencies

  # RC TO DO:
  # - update subsetting to use the prd argument, replace min year/ max year
  # update to have water year just be a sorting? make the sorting flexible to mm/dd arguments

  #-- Handle if no min/max passed
  # if (is.null(minyear)) { minyear <- year(start(hgdata))}
  # if (is.null(maxyear)) { maxyear <- year(end(hgdata))}

  #-- Subset by date
  prd <- rvn_get_prd(hgdata, prd)
  hgdata <- hgdata[prd]

  maxyear <- year(end(hgdata))

  #-- Aggregate to Quantiles by Day
  # (Uses maxyear as placeholder for year)
  monthday <- as.Date(paste0(toString(maxyear), "-", month(hgdata), "-", day(hgdata)))
  if (water_year) {
    # Year is arbitrary, so subtract a year post-Oct to create water years
    # RC to do - do this calculation based on yday to get support for month and day parameters in water year
    ## would support June 15th water year, for example
    monthday[month(monthday) > mm] = monthday[month(monthday) > mm] - years(1)
  }
  qdat <- xts(aggregate(hgdata, by=monthday, quantile,
                        probs=c(Qlower, .5, Qupper), na.rm=TRUE))

  return(qdat)
}
