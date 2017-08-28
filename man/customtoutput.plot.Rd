\name{customoutput.plot}
\alias{customoutput.plot}
\title{Plot Raven Custom Output}
\usage{
customoutput.plot(cust,IDs=NULL,prd=NULL)
}
\arguments{
\item{cust}{custom output object from custom.read}
\item{IDs}{(optional) array of HRU IDs, subbasin IDs, HRU Group names/IDs to include in plots}
\item{prd}{(optional) period to use in plotting}
}

\description{
customoutput.plot is used to plot the custom output from Raven
}
\details{
customoutput.plot plots the custom output from a Raven model. The custom output should be first read in using the custom.read function.
}

\seealso{
\code{\link{customoutput.plot}} for plotting custom output

See also \href{http://www.civil.uwaterloo.ca/jrcraig/Default.htm}{James R. Craig's research page} for software downloads, including the \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
}
\examples{
  # read in custom output
custom1 <- customoutput.plot('model_output/PRECIP_Daily_Average_BySubbasin.csv')

  # plot custom output
customoutput.plot(custom1)
customoutput.plot(custom1,IDs=seq(23,27),prd="2015-10-01/2017-10-01")
}
\keyword{Raven}
\keyword{plot}
\keyword{custom}
\keyword{output}
