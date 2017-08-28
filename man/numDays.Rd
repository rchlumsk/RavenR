\name{numDays}
\alias{numDays}
\title{Number of Days between two dates}
\usage{
numDays(date1,date2)
}
\arguments{
\item{date1}{first day, date format}
\item{date2}{second day, date format}
}

\value{
\item{int}{number of days between the two days}
}

\description{
numDays is used to calculate the number of days in the month; handles leap years
}

\seealso{
\code{\link{numDaysMonth}} for calculating the number of days in a month
}
\examples{
numDays(as.Date("2017-02-05"),as.Date("2017-02-12"))
# 7
}
\keyword{days}
\keyword{number}
