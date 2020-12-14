#' @title Forcings Data from Raven
#'
#' @description
#' A dataset formatted to the xts package, read in by the forcings.read function.
#' The dataset contains the typical columns from the Raven outputted
#' ForcingFunctions.csv file, available for download in the Raven
#' Tutorials (linked below).
#'
#'@format rvn_forcing_data is a data frame with two object
#' \describe{
#'   \item{forcings}{various forcing functions and related output from Raven model}
#'   \item{units}{units associated with each variable in forcings}
#' }
#'
#' rvn_forcing_data$forcings is an xts (time series) object with 731 rows and
#' 21 variables, containing data from 2002-10-01 to 2004-09-30. The details of
#' each forcing function can be found in the Raven Manual
#' \itemize{
#'  \item{day_angle}
#'  \item{rain}
#'  \item{snow}
#'  \item{temp}
#'  \item{temp_daily_min}
#'  \item{temp_daily_max}
#'  \item{temp_daily_ave}
#'  \item{temp_monthly_min}
#'  \item{temp_monthly_max}
#'  \item{air_dens}
#'  \item{air_pres}
#'  \item{rel_hum}
#'  \item{cloud_cover}
#'  \item{ET_radiation}
#'  \item{SW_radiation}
#'  \item{LW_radiation}
#'  \item{wind_vel}
#'  \item{PET}
#'  \item{OW_PET}
#'  \item{daily_correction}
#'  \item{potential_melt}
#' }
#'
#' The Nith River model can be downloaded from the Raven Tutorials (tutorial #2)
#' \url{http://www.civil.uwaterloo.ca/jrcraig/Raven/Downloads.html}
#'
#' @seealso \code{\link{rvn_forcings_read}} for reading in forcing functions output files
#' @seealso \code{\link{rvn_forcings_plot}} for plotting forcing functions in a convenient way
#'
#'
"rvn_forcing_data"
