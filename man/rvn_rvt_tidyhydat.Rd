% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_rvt_tidyhydat.R
\name{rvn_rvt_tidyhydat}
\alias{rvn_rvt_tidyhydat}
\title{EC Streamgauge File Conversion from tidyhydat}
\usage{
rvn_rvt_tidyhydat(
  indata,
  subIDs,
  prd = NULL,
  stnNames = NULL,
  write_redirect = FALSE,
  flip_number = FALSE,
  rd_file = "flow_stn_redirect_text.rvt",
  filename = NULL
)
}
\arguments{
\item{indata}{tibble of WSC flow data from tidyhydat's hy_daily_flows() function}

\item{subIDs}{vector of subbasin IDs to correspond to the stations in indata}

\item{prd}{(optional) data period to use in .rvt file}

\item{stnNames}{(optional) character vector of alternative station names to use}

\item{write_redirect}{(optional) write the :RedirectToFile commands in a separate .rvt file}

\item{flip_number}{(optional) put the subID first in the .rvt filename}

\item{rd_file}{(optional) name of the redirect file created (if write_redirect = TRUE)}

\item{filename}{specified name of file(s) to write to (optional)}
}
\value{
\item{TRUE}{return TRUE if the function is executed properly}
}
\description{
rvn_rvt_tidyhydat converts Environment Canada historical streamgauge data,
accessed via the tidyhydat package, into .rvt format files usable in
Raven.
}
\details{
This function takes a single flow tibble generated from tidyhydat and converts the flow data for
each station in the file into .rvt formatted files for a Raven model. If
multiple stations exist in the .csv file, multiple observation files are created

subIDs is required and should correspond to the subID to be used in the .rvt
file for each station in the ff file, in the order in which it will be read
in.

prd is used by the xts formatted-data to restrict the data reported in .rvt
files, for each station, to this period. The prd should be defined in
"YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
empty time series (i.e. non-overlapping time periods), an error will be
thrown.

stnNames is an optional character vector to replace the EC station codes
found in the HYDAT database. If supplied, the vector must be of the same length
as the number of stations supplied and the subIDs vector. If not supplied,
the EC station codes will be used. Note that this does not impact model
function, only filename readability and station recognition.

write_redirect will print out the :RedirectToFile commands in a separate
file called, "flow_stn_redirect_text.rvt". These commands can be copied into
the main model's .rvt file to redirect to the produced time series files.

flip_number is a useful option to place the subID first in the filename.
This is often cleaner for organizing files in a folder, since the
alphabeticized order is not dependent on the station name, and the observed
files will be in one set.

The function will write to name generated from the station name(s), otherwise
the .rvt filename may be specified with the filename argument (full path to
the filename, including .rvt extension). If multiple stations are provided,
the filename argument may be a vector of filenames.
}
\examples{

# note: example modified to avoid using tidyhydat directly, uses saved
## tidyhydat data from RavenR package sample data
# library(tidyhydat)
stations <- c("05CB004","05CA002")

# Gather station data/info using tidyhydat functions
# hd <- tidyhydat::hy_daily_flows(station_number = stations,
#  start_date = "1996-01-01", end_date = "1997-01-01")
data(rvn_tidyhydat_sample)
hd <- rvn_tidyhydat_sample

# station_info <- hy_stations(stations)

tf1 <- file.path(tempdir(), "station1.rvt")
tf2 <- file.path(tempdir(), "station2.rvt")

# Create RVT files
rvn_rvt_tidyhydat(hd, subIDs=c(3,11),
  filename=c(tf1,tf2))

}
