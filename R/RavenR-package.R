#'
#' @title RavenR
#' @description Contains functions to perform pre- and post-processing of RavenR files.
#' @docType package
#' @name RavenR-package
#' @details This package provides a number of types of useful functions, including:
#' \itemize{
#' \item{reading in \strong{Raven output files} (including ForcingFunctions, Hydrographs, etc.)}
#' \item{processing some file types into \strong{Raven format} (e.g. ECflow.rvt, ECmet.rvt)}
#' \item{flow-based \strong{diagnostics} and plot functions}
#' \item{other useful plotting functions (e.g. forcings.plot, flow duration curve, dyGraphs)}
#' \item{useful utilities for time series and hydrologic functions (e.g. apply.wyearly)}
#' }
#'
#' @importFrom lubridate year month day date yday
#' @importFrom xts xts apply.monthly apply.yearly apply.daily period.apply xtsAttributes
#' @importFrom zoo index coredata rollapply
#'
#' @importFrom grDevices col2rgb colours
#' @importFrom graphics abline axis grid legend lines mtext par plot polygon text title
#' @importFrom stats ecdf rnorm sd time
#' @importFrom utils read.csv read.table write.csv
#'
#' @seealso The package \href{https://github.com/rchlumsk/RavenR}{GitHub page},
#' \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#'
NULL
