#' @title tidyhydat sample data for RavenR package
#'
#' @description
#' A dataset downloaded using the tidyhydat package for two stations, between 1996-01-01 and
#' 1997-01-01. Additional details on the tidyhydat package and data formats can be found at the
#' \href{https://github.com/ropensci/tidyhydat}{tidyhydat github page}.
#'
#' Note that this sample is provided to avoid loading the tidyhydat package and requiring the download_hydat()
#' function in CRAN testing of examples.
#'
#' Additional information on data provided by the Water Survey of Canada may be found on the WSC webpage.
#'
#'@format rvn_tidyhydat_sample is a tibble with 671 rows, and 5 columns.
#' \describe{
#'   \item{STATION_NUMBER}{station number from WSC stations in HYDAT database}
#'   \item{Date}{date in YYYY-MM-DD format}
#'   \item{Parameter}{Code indicating the parameter provided in the given row as either flow or level data}
#'   \item{Value}{the value of the parameter provided (flow or level)}
#'   \item{Symbol}{additional data flags provided by WSC}
#' }
#'
#' @seealso \code{\link{rvn_rvt_tidyhydat}} for writing rvt files from tidyhydat data
#'
"rvn_tidyhydat_sample"
