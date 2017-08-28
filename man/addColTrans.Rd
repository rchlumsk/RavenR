\name{addColTrans}
\alias{addColTrans}
\title{Add Transparency to Colours}
\usage{
addColTrans(colour,trans)
}
\arguments{
\item{colour}{time series containing columns you wish to reseasonalize. xts object}

\item{trans}{integer describing the degree of transparency, from ~200 (slightly transparent) to <10 (very transparent)}
}

\value{
\item{res}{returned updated colour code with transparency}
}

\description{
addColTrans is used to adjust colour codes to introduce transparency
}

\seealso{
See original code on post in Stack Overflow \href{http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r}{making plot points transparent in R}
}
\examples{
# plot randomly distributed data
plot(rnorm(20),col='black')

# create a transparent blue colour for plotting
mycol <- addColTrans('blue',100)

# plot more random points in transparent blue colour
points(rnorm(20),col=mycol)
}
\keyword{colour}
\keyword{transparency}

