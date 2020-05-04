% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_annual_peak.R
\name{rvn_annual_peak}
\alias{rvn_annual_peak}
\title{Annual Peak Comparison}
\usage{
rvn_annual_peak(sim, obs, rplot = T, add_line = T, add_r2 = F)
}
\arguments{
\item{sim}{time series object of simulated flows}

\item{obs}{time series object of observed flows}

\item{rplot}{boolean whether to generate plot (default TRUE)}

\item{add_line}{optionally adds a 1:1 line to the plot for reference
(default TRUE)}

\item{add_r2}{optionally computes the R2 and adds to plot (default FALSE)}

\item{axis_zero}{optionally sets the minimum volume on axes to zero (default
FALSE)}
}
\value{
\item{df_peak}{data frame of the calculated peaks}
}
\description{
rvn_annual_peak creates a plot of the annual observed and simulated peaks.
}
\details{
This function creates a scatterplot of the annual observed and simulated
peaks, calculated for each available water year of data (Oct 1st hardcoded)
within the two series provided. Note that the calculation uses the peak
magnitude of simulated and observed series in each water year, without
considering the timing of the events in each series.

The sim and obs should be of time series (xts) format and are assumed to be
of the same length and time period. The flow series are assumed to be daily
flows with units of m3/s.

The R2 diagnostic is calculated for a fit with no intercept (in a perfect
fit the points are identical, and intercept is automatically zero).

Note that a plot title is purposely omitted in order to allow the automatic
generation of plot titles.
}
\examples{

# load sample hydrograph data, two years worth of sim/obs
data(hydrograph_data)
sim <- hydrograph_data$hyd$Sub36
obs <- hydrograph_data$hyd$Sub36_obs

# create a plot of annual peaks with default options
rvn_annual_peak(sim,obs,rplot=T,add_line=T,add_r2=F,axis_zero=F)

# plot with r2 and axes to zero; store results
peak_df <- rvn_annual_peak(sim,obs,rplot=T,add_line=T,add_r2=T,axis_zero=T)

# store results without plotting
peak_df <- rvn_annual_peak(sim,obs,rplot=F)

}
\seealso{
\code{\link{rvn_annual_volume}} to create a scatterplot of annual flow
volumes \code{\link{rvn_annual_peak_event}} to consider the timing of peak
events

See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
Craig's research page} for software downloads, including the
\href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
}
\keyword{Raven}
\keyword{annual}
\keyword{diagnostics}
\keyword{peak}