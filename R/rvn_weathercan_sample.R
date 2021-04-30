#' @title weathercan sample data for RavenR package
#'
#' @description
#' A dataset downloaded using the weathercan package for the 'KAMLOOPS A' station (station id 51423),
#' between 2016-10-01 and 2019-09-30. Additional details on the weathercan package and data formats can be found at the
#' \href{https://github.com/ropensci/weathercan}{weathercan github page}.
#'
#' Note that this sample is provided to avoid loading the weathercan package while compiling and testing the package.
#'
#' Additional information on data provided by Environment Canada can be found on the Historical Data portal.
#'
#' @format rvn_weathercan_sample is a tibble with 1095 rows, and 37 columns.
#'
#' @seealso \code{\link{rvn_rvt_ECmet}} for writing rvt files from weathercan data
#'
#' @source \url{https://climate.weather.gc.ca/historical_data/search_historic_data_e.html}
#'
"rvn_weathercan_sample"
