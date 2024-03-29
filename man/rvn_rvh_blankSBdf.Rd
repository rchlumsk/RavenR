% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_rvh_blankSBdf.R
\name{rvn_rvh_blankSBdf}
\alias{rvn_rvh_blankSBdf}
\title{Generate Blank Raven SubBasin DataFrame}
\usage{
rvn_rvh_blankSBdf(nSubBasins = 1)
}
\arguments{
\item{nSubBasins}{Number of SubBasins in model, used to determine number of rows in table (default = 1)}
}
\value{
data.frame of blank SubBasin properties to be filled in by user
}
\description{
Generates a blank data frame for Raven subbasin properties. Compatible with \code{\link{rvn_rvh_write}}.
}
\details{
The subbasin names are provided as 'sub00x', where x is the basin ID. The padding is determined
from the number of subbasins. The downstream IDs are generated to assume a linear downstream progression,
with an outlet at the terminal subbasin ID, which can be modified after the data frame is created.
}
\examples{
SBtable <- rvn_rvh_blankSBdf(nSubBasins = 3)
SBtable

}
\seealso{
\code{\link{rvn_rvh_blankHRUdf}} to generate blank HRU data frame
}
\author{
Leland Scantlebury
}
