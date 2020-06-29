#' Forcings Data from Raven
#'
#' A dataset formatted to the xts package, read in by the forcings.read function.
#' The dataset contains the typical columns from the Raven outputted
#' ForcingFunctions.csv file, available for download in the Raven
#' Tutorials (linked below).
#'
#'@format forcing.data is a data frame with two object
#' \describe{
#'   \item{forcings}{various forcing functions and related output from Raven model}
#'   \item{units}{units associated with each variable in forcings}
#' }
#'
#' forcing.data$forcings is an xts (time series) object with 731 rows and
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
#'  \item{air.dens}
#'  \item{air.pres}
#'  \item{rel.hum}
#'  \item{cloud.cover}
#'  \item{ET.radiation}
#'  \item{SW.radiation}
#'  \item{LW.radiation}
#'  \item{wind.vel}
#'  \item{PET}
#'  \item{OW.PET}
#'  \item{daily.correction}
#'  \item{potential.melt}
#' }
#'
#' The Nith River model can be downloaded from the Raven Tutorials (tutorial #2)
#' \url{http://www.civil.uwaterloo.ca/jrcraig/Raven/Downloads.html}
#'
#' @seealso \code{\link{forcings.read}} for reading in forcing functions output files
#' @seealso \code{\link{forcings.plot}} for plotting forcing functions in a convenient way
#'
#'
"forcing.data"
