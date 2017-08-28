\name{numDaysMonth}
\alias{numDaysMonth}
\title{Number of Days in Month}
\usage{
numDaysMonth(date)
}
\arguments{
\item{date}{object in date format}
}

\value{
\item{int}{number of days in the month}
}

\description{
numDaysMonth is used to calculate the number of days in the month; handles leap years
}

\seealso{
\code{\link{numDays}} for calculating the number of days between two dates

See original code on post in Stack Overflow \href{http://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-r}{find the number of days in a month}
}
\examples{
numDaysMonth(as.Date("2016-02-05"))
# 29

numDaysMonth(as.Date("2017-01-17"))
# 31
}
\keyword{days}
\keyword{month}

