% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_rvh_query.R
\name{rvn_rvh_query}
\alias{rvn_rvh_query}
\title{Queries RVH object for subbasins and HRUs of interest}
\usage{
rvn_rvh_query(rvh = NULL, subbasinID = NULL, condition = "upstream_of")
}
\arguments{
\item{rvh}{rvh object as returned by \code{\link{rvn_rvh_read}}}

\item{subbasinID}{subbasinID of basin of interest, as character or integer}

\item{condition}{condition applied to the query}
}
\value{
rvh object in same format, but queried to condition and all features (SBtable, HRUtable, SBnetwork) updated.
}
\description{
Queries the RVH object for subbasins or HRUs that are upstream of, downstream of,
or the opposite of those conditions, for a given subbasin ID.
}
\details{
Based on the definition of subbasins by their outlets in Raven, it is assumed here that 'upstream' includes
the specified subbasin (i.e. everything upstream of subbasin X includes subbasin X as well), and 'downstream'
of subbasin X does not include subbasin X. This is different from the default behaviour of \code{igraph}, which
includes the specified subbasin in either query.
}
\note{
Raven has capabilities for creating subbasin and HRU groups that meet certain criteria as well, consider
reviewing the `:PopulateSubbasinGroup`, `:PopulateHRUGroup`, and other commands in Section A.3.2 of the
Raven User's Manual.
}
\examples{
# load example rvh file
nith <- system.file("extdata","Nith.rvh",package = "RavenR")
rvh <- rvn_rvh_read(nith)

# plot full watershed with igraph library
plot(rvh$SBnetwork)

# query all subbasins upstream of basin 39, plot
rvh_upstream_of_39 <- rvn_rvh_query(rvh, subbasinID=39, condition="upstream_of")
plot(rvh_upstream_of_39$SBnetwork)

# query of HRUs downstream of basin 39
rvn_rvh_query(rvh, subbasinID=39, condition="downstream_of")$SBtable

}
\seealso{
\code{\link{rvn_rvh_write}} to write contents of the generated (and usually modified HRU and SubBasin tables)
\code{\link{rvn_rvh_read}} to read a Raven RVH file into R
}
