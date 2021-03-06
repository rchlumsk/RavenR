% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_rvt_ECmet.R
\name{rvn_rvt_ECmet}
\alias{rvn_rvt_ECmet}
\title{EC Climate Station File Conversion}
\usage{
rvn_rvt_ECmet(
  metdata,
  filename = NULL,
  prd = NULL,
  stnName = NULL,
  forcing_set = 1,
  prefix = "met_",
  write_redirect = FALSE,
  write_stndata = FALSE,
  rd_file = "met_redirects.rvt",
  stndata_file = "met_stndata.rvt"
)
}
\arguments{
\item{metdata}{EC meteorological data from one or more stations (e.g., from weathercan::weather_dl())}

\item{filename}{specified name of file to write to (optional)}

\item{prd}{(optional) data period to use in .rvt file}

\item{stnName}{(optional) station name to use (instead of name in file)}

\item{forcing_set}{(optional) specifies the set of forcings to print to file}

\item{prefix}{(optional) prefixes the file name (default: "met_")}

\item{write_redirect}{(optional) write the :RedirectToFile commands in a
separate .rvt file}

\item{write_stndata}{(optional) write the gauge data to a separate .rvt file}

\item{rd_file}{(optional) name of the redirect file created (if write_redirect = TRUE)}

\item{stndata_file}{(optional) name of the station data file created (if write_stndata = TRUE)}
}
\value{
\item{TRUE}{return TRUE if the function is executed properly}
}
\description{
rvn_rvt_ECmet converts Environment Canada historical meteorological data for a
given station into the .rvt format usable in Raven.
}
\details{
The function prints in the :MultiData format; the particular set of forcings to
print can be set with the forcing_set command. The data should be downloaded
with missing days included. The download website is linked below.

The function will write to name generated from the station name, otherwise
the .rvt filename may be specified with the filename argument (full path to
the filename, including .rvt extension).

prd is used by the xts formatted-data to restrict the data reported in .rvt
files, for each station, to this period. The prd should be defined in
"YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
empty time series (i.e. non-overlapping time periods), an error will be
thrown.

stnName can be supplied to overwrite the station name that is otherwise
obtained from the Station Name field in the climate file. Spaces in raw Station
Names will be replaced with underscores.

prefix can be used to add a prefix to the .rvt file name, ("met_" by default)
which may be useful in organizing multiple climate data files.

forcing_set specifies the set of forcings to print to .rvt file. Currently
there are only two available sets. A value of 1 prints total precipitation,
and 2 splits the precipitation into rainfall and snowfall. In some cases the
EC data provides only total precipitation, which is a good check to make for
the particular climate station before printing rvt files. Both sets
currently print max and min daily temperature. Future extensions to this
function may provide more options for forcing sets.

write_redirect will print out the :RedirectToFile commands in a separate
file, met_redirects.rvt. These commands can be copied into the main model's
.rvt file to redirect to the produced time series files. The function will
append to the file if it already exists, meaning that this works for
iterations of this function.

write_stndata wil print out the gauge metadata to file (met_stndata.rvt) in
the .rvt format, which is required to include a meterological station in
Raven. The function will append to the file if it already exists, meaning
that this works for iterations of this function.

perform.qc is currently under construction and is not yet available; setting
to TRUE will result in an warning.

The function has several built-in data quality checks. These include:

* checking that all supplied files are for the same climate station
* ensuring;
the timestep (data resolution) is the same in each file
* automatically;
combining time series and ensuring there are no gaps in the data supplied
(i.e. time gaps, not missing values);  and
* check for missing data and issuing a warning that post-processing will be required

Note: Data quality is not assessed in this package, such as consistency between
minimum and maximum temperatures. Subdaily data is not currenty supported.

Note: this function is designated to use data from the weathercan package. The
weathercan package is external to RavenR and is not an explicit dependency
of RavenR.
}
\examples{

# note: example modified to avoid using weathercan directly, uses saved
## weathercan data from RavenR package sample data
# library(weathercan)
# kam <- weather_dl(station_ids = 51423,
#                   start = "2016-10-01", end = "2019-09-30", interval="day")
data(rvn_weathercan_sample)
kam <- rvn_weathercan_sample

# basic use, override filename to temporary file
# default forcing_set (PRECIP, MAX TEMP, MIN TEMP)
rvn_rvt_ECmet(metdata = kam, forcing_set = 1,
  filename = file.path(tempdir(), "rvn_rvt_ECmetfile1.rvt"))

# use the second forcing set instead
# default forcing_set (PRECIP, MAX TEMP, MIN TEMP)
rvn_rvt_ECmet(metdata = kam, forcing_set = 2,
  filename = file.path(tempdir(), "rvn_rvt_ECmetfile2.rvt"))

}
\seealso{
\code{\link{rvn_rvt_wsc}} to convert WSC flow gauge data to Raven format

Download Environment Canada Historical weather data from (climate.weather.gc.ca), or use the
`weathercan` package to access this data through R.
}
