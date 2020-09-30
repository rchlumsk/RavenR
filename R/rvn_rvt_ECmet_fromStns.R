#' @title Generate Raven .rvt files from EC Station data
#'
#' @description
#' Uses the weathercan package to download data and write the .rvt file using teh data.
#'
#' @param station_tbl tibble of Station info from weathercan::stations_search(). Can also be data.frame with station_ids column.
#' @param start Date/Character. The start date of the data in YYYY-MM-DD format (applies to all stations_ids).
#' @param end Date/Character. The end date of the data in YYYY-MM-DD format (applies to all station_ids).
#' @param interval Character. Interval of the data, one of "hour", "day", "month".
#' @param forcing_set Integer. Specifies the set of forcings to print to file, see \link{rvn_rvt_ECmet} (optional)
#' @param ... Additional arguments passed to \link{rvn_rvt_ECmet}
#'
#' @return TRUE returns TRUE if function executed successfully.
#' @author Leland Scantlebury
#'
#' @examples
#' # Built for pipes
#' weathercan::stations_search("Waterloo", interval="day") %>%
#'             filter(station_id %in% c(4832, 4831)) %>%
#'             rvn_rvt_ECmet_fromStns(start="1990-10-01", end="2000-09-30", interval="day")
#' @export rvn_rvt_ECmet_fromStns
#' @importFrom weathercan weather_dl
rvn_rvt_ECmet_fromStns <- function(station_tbl, start, end, interval, forcing_set = 1, ...)
{

  #-- Get data
  datatbl <- weathercan::weather_dl(station_ids = station_tbl$station_id,
                                    start = start, end = end, interval = interval)

  #-- TODO: Error handling if no data

  #-- Run rvn_rvt_ECmet
  rvn_rvt_ECmet(metdata = datatbl, forcing_set =  ...)

  return(TRUE)
}
