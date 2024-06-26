% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rvn_rvi_commandupdate.R
\name{rvn_rvi_commandupdate}
\alias{rvn_rvi_commandupdate}
\title{Update command in Raven input file}
\usage{
rvn_rvi_commandupdate(
  filename = NULL,
  command = NULL,
  value = NULL,
  outputfile = NA
)
}
\arguments{
\item{filename}{the name of the .rvi file (with .rvi extension included), either relative
to the working directory or absolute.}

\item{command}{the rvi command with preceeding colon to update}

\item{value}{value of the command to update}

\item{outputfile}{if writing to a new file, otherwise filename is overwritten}
}
\value{
{returns \code{TRUE} if run successfully}
}
\description{
Updates the provided rvi file with the command and value provided.
}
\examples{
  # load example rvi file
  ff <- system.file("extdata","Nith.rvi", package="RavenR")
  tf <- tempfile(fileext=".rvi")

  rvn_rvi_commandupdate(filename=ff,
  command=":StartDate",
  value="2022-10-01 00:00:00",
  outputfile=tf)

}
\seealso{
\code{\link{rvn_rvi_read}} to read and process rvi files with RavenR
}
\author{
James R. Craig, University of Waterloo
}
