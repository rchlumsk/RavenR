#' Watershed Storage Data from Raven
#'
#' A dataset formatted to the xts package, read in by the watershed.read function.
#' The dataset contains the typical columns from the Raven outputted
#' WatershedStorage.csv file, available for download in the Raven
#' Tutorials (linked below).
#'
#'@format watershed.data is a data frame with two object
#' \describe{
#'   \item{watershed.storage}{various storage variable states outputted from the
#'   Raven model}
#'   \item{units}{units associated with each variable in watershed.storage}
#' }
#'
#' watershed.data$watershed.storage is an xts (time series) object with 731 rows and
#' 19 variables, containing data from 2002-10-01 to 2004-09-30. The details of
#' each watershed storage state can be found in the Raven Manual
#' \itemize{
#'  \item{rainfall}
#'  \item{snowfall}
#'  \item{Channel.Storage}
#'  \item{Reservoir.Storage}
#'  \item{Rivulet.Storage}
#'  \item{Surface.Water}
#'  \item{Cum..Losses.to.Atmosphere..mm.}
#'  \item{Ponded.Water}
#'  \item{Soil.Water.0}
#'  \item{Soil.Water.1}
#'  \item{Soil.Water.2}
#'  \item{Snow.Melt..Liquid..mm.}
#'  \item{Snow}
#'  \item{Canopy}
#'  \item{Canopy.Snow}
#'  \item{Total}
#'  \item{Cum..Inputs..mm.}
#'  \item{Cum..Outflow..mm.}
#'  \item{MB.Error}
#' }
#'
#' The Nith River model can be downloaded from the Raven Tutorials (tutorial #2)
#' \url{http://www.civil.uwaterloo.ca/jrcraig/Raven/Downloads.html}
#'
#' @seealso \code{\link{watershed.read}} for reading in watershed storage output files
#'
"watershed.data"
