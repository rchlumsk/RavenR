#' @title weathercan sample metadata for RavenR package
#'
#' @description
#' A dataset downloaded using the \pkg{weathercan} package function \code{stations_search} for stations
#' within 150 km of Merritt, BC. Additional details on the weathercan package and data formats can be found at the
#' \href{https://github.com/ropensci/weathercan}{weathercan github page}.
#'
#' Note that this sample is provided to avoid loading the \pkg{weathercan} package while compiling and testing the package.
#'
#' Additional information on data provided by Environment Canada can be found on the Historical Data portal.
#'
#' @format rvn_weathercan_sample is a tibble with 3 rows, and 17 columns.
#'
#' @seealso \code{\link{rvn_met_recordplot}} for checking the record of meteorological data. This function example
#'  also has the original code used to create the rvn_weathercan_metadata_sample data set.
#'
#' @source Historical Climate Data from Environment Canada (climate.weather.gc.ca) via `weathercan` package
#'
"rvn_weathercan_metadata_sample"
