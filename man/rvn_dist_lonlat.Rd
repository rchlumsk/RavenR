% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_utilities.R
\name{rvn_dist_lonlat}
\alias{rvn_dist_lonlat}
\title{Calculate distance from long/lat}
\usage{
rvn_dist_lonlat(p1, p2, method = "haversine", r = 6378137)
}
\arguments{
\item{p1}{longitude/latitude of point(s); can be a vector of two numbers, or a matrix of 2 columns (long/lat).}

\item{p2}{second point in same format as \code{p1}}

\item{method}{calculation method as either \code{haversine} (default) or \code{vincentysphere}}

\item{r}{radius of the Earth in metres (default 6378137)}
}
\value{
a vector of calculated distances (length of vector based on input)
}
\description{
Calculates distance between points based on a set of long/lat coordinates.
}
\details{
Calculates distance in metres based on the longitude and latitude of two or more sets of points.

The function uses either the Haversine or Vincenty Sphere methods to calculate the distances.
}
\note{
Function is based on modifications from the \href{https://cran.r-project.org/package=geosphere}{geosphere package}
scripts for \code{distHaversine} and \code{distVincentySphere}.
}
\examples{
# calculate distance from Engineering 2 (p1) to Graduate House (p2) at the University of Waterloo
p1 <- c(-80.5402891965711,43.47088594350457)
p2 <- c(-80.54096577853629,43.46976096704924)
rvn_dist_lonlat(p1, p2)

# distance from University of Waterloo to Windsor
p2 <- c(-83.02099905916948,42.283371378771555)
rvn_dist_lonlat(p1, p2)

}
