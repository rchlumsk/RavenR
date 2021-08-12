#' @title Rvt Mappings Data
#'
#' @name rvn_rvt_mappings_data
#'
#' @description
#' A list of lists that has information for use in the RavenR rvt-related files.
#'
#' Additional information on Raven variables can be found in the Raven User's Manual, available from
#' \url{http://raven.uwaterloo.ca/Downloads.html}
#'
#' @format \code{rvn_rvt_mappings_data} is a list with four lists.
#' \describe{
#'   \item{rvt_mapping}{list: data formats for all rvt file types}
#'   \item{rvt_data_type_mapping}{list: data types permitted in Raven, and associated units}
#'   \item{rvn_met_raven_mapping}{list: meteorological forcing functions permitted in Raven, and associated units}
#'   \item{rvt_met_mapping_weathercan}{list: mapping of weathercan variable names to Raven variables}
#' }
#'
#' @seealso \code{\link{rvn_rvt_read}} for reading in rvt files
#' @seealso \code{\link{rvn_rvt_write}} and \code{\link{rvn_rvt_write_met}} for writing data to rvt files.
#'
NULL
