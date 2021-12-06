#'
#' @title RavenR
#' @description Contains functions to perform pre- and post-processing of RavenR files.
#' @docType package
#' @name RavenR-package
#' @importFrom Rcpp sourceCpp
#' @useDynLib RavenR
#' @details This package provides a number of types of useful functions, including:
#' \itemize{
#' \item{reading in \strong{Raven output files} (including ForcingFunctions, Hydrographs, etc.)}
#' \item{processing some file types into \strong{Raven format}}
#' \item{flow-based \strong{diagnostics} and plot functions}
#' \item{other useful plotting functions (e.g. rvi process plot, spaghetti plot, dyGraphs, network plots)}
#' \item{useful utilities for time series and hydrologic functions (e.g. rvn_apply_wyearly)}
#' }
#'
#' @seealso The package on \href{https://cran.r-project.org/package=RavenR}{CRAN} and
#' the development version on \href{https://github.com/rchlumsk/RavenR}{GitHub}.
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://raven.uwaterloo.ca/}{Raven webpage}.
#'
#'
NULL
