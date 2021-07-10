#' @title Custom Output Data from Raven
#'
#' @description
#' A dataset formatted to the xts package, read in by the \code{\link{rvn_custom_read}} function.
#' The dataset contains average SNOW for each HRU in the Nith river model, available
#' for download in the Raven Tutorials (linked below).
#'
#' Note that this data set cannot be used with \code{\link{rvn_custom_output_plot}} as the file name
#' information is not available in this data format. Please refer to the example in the plotting function
#' to use the sample data file directly, which includes the filename information.
#'
#' The Nith River model can be downloaded from the Raven Tutorials (tutorial #2)
#' \url{http://www.civil.uwaterloo.ca/jrcraig/Raven/Downloads.html}
#'
#' @seealso \code{\link{rvn_custom_read}} for reading in custom output files
#' @seealso \code{\link{rvn_custom_output_plot}} for plotting custom output
#'
#' @format A data frame with 730 rows, containg data for 32 HRUs from 2002-10-01 to 2004-09-29
#'
#' @examples
#' # Preview data
#' head(rvn_custom_data)
#'
"rvn_custom_data"
