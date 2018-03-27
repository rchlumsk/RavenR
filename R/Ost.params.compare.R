#' Compare parameters between Ostrich jobs
#'
#' Ost.params.compare is used to compare parameter values between different
#' Ostrich jobs.
#'
#' Ost.params.compare is used to compare parameter values between different
#' Ostrich jobs of the same format (the number of metrics and parameter should
#' be the same between each file). This function is useful in determining which
#' parameters changed values by a large margin between two or more different
#' calibration jobs (based on the optimal objective function trial). In order
#' to compare the ensemble of equifinal parameter values between different
#' jobs, extra analysis for data returned from the Ost.bestparams function
#' should be done.
#'
#' ffs is the vector of files to be read in and compared, which can be relative
#' or absolute pathways. The function will stop if any of these cannot be
#' found. It should be emphasized that the format between each of these files
#' needs to be the same in terms of number of metrics and parameters (i.e.
#' number of columns). The number of rows (calibration runs) can be different,
#' since this function will only extract the best run to examine.
#'
#' num.metrics refers to the number of fit metrics used in the Ostrich
#' calibration, of which there should be at least one. If this value is
#' supplied, the function will remove the metric columns from the returned
#' data.
#'
#' find.dev will return the indices of columns from the returned data that show
#' a large variation. Note that if this boolean is true, the returned object is
#' not just the dataset but a list of objects (see example below).
#'
#' num.sd indicates the number of standard deviations used in the definition of
#' a 'large' variation between parameter values. To assume a normal
#' distribution and 95 percent interval, one can use 1.96 here.
#'
#' @param ffs vector of Ostrich data files to compare
#' @param num.metrics number of fit metrics used in Ostrich; will remove the
#' metrics if supplied
#' @param find.dev boolean for finding parameter with large deviations between
#' jobs
#' @param num.sd number of standard deviations to define a large deviation
#' between jobs; default 1
#' @return \item{Ost.paramcompare}{data frame of parameter values and
#' statistics} \item{dev.ind}{vector of indices showing parameter with some
#' variation between jobs}
#' @seealso \code{\link{Ost.read}} for reading in Ostrich files
#'
#' \code{\link{Ost.plot}} for creating various plots with Ostrich data
#'
#' \code{\link{Ost.bestparams}} for obtaining the best parameter sets from
#' Ostrich data
#'
#' Download Ostrich at
#' \href{http://www.eng.buffalo.edu/~lsmatott/Ostrich/OstrichMain.html}{Shawn
#' Matott's web page}
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for other software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Ostrich calibration parameter
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' ff1 <- "1_OstModel0.txt"
#' ff2 <- "2_OstModel0.txt"
#' ff3 <- "3_OstModel0.txt"
#' ffm <- c(ff1,ff2,ff3)
#'
#' mydata <- Ost.params.compare(ffm,find.dev=F)
#' head(mydata)
#'
#' mydata <- Ost.params.compare(ffm,find.dev=F)
#' mydata$dev.ind  # indices where some job had largely different value(s)
#' head(mydata$Ost.paramcompare)  # data frame of parameter values and statistics
#'
#' # show parameter values with deviations
#' mydata$Ost.paramcompare[,mydata$dev.ind]
#' }
#'
#' @export Ost.params.compare
Ost.params.compare <- function(ffs=NULL,num.metrics=NA,find.dev=F,num.sd=1) {

  # basic checks
  if (is.null(ffs)) {
    stop("ffs is required to have at least two different parameter sets to compare")
  }
  N <- length(ffs)
  for (i in 1:N) {
    if (!(file.exists(ffs[i]))) {
      stop(sprintf("File %s cannot be found",ffs[i]))
    }
  }
  if (num.metrics < 1 & !(is.na(floor(num.metrics)))) { stop("Cannot have less than 1 metric; check num.metrics.")}


  # create vector of data objects
  # assume same dimensions for each
  Ost.data <- read.table(ffs[1],header=T)
  if (!(is.na(floor(num.metrics)))) {
    Ost.data <- Ost.data[,-(3:(floor(num.metrics)+2))] # remove metrics aside from obj function
  }
  Ost.data <- Ost.data[,-1]
  best <- Ost.data[Ost.data[,1] == max(Ost.data[,1]),]
  n <- length(best)
  cc <- colnames(Ost.data)

  # array for holding data
  # array(NA,c(n,m,N))
  mat <- matrix(NA,nrow=N,ncol=n)

  # collect all data
  for (i in 1:N) {
    Ost.data <- read.table(ffs[i],header=T)
    # remove metrics
    if (!(is.na(floor(num.metrics)))) {
      Ost.data <- Ost.data[,-(3:(floor(num.metrics)+2))] # remove metrics aside from obj function
    }
    # remove run
    Ost.data <- Ost.data[,-1]
    # obtain the best trial
    best <- as.numeric(Ost.data[Ost.data[,1] == max(Ost.data[,1]),])
    # assign into structure
    mat[i,] <- best
  }

  # create data frame
  df <- data.frame(mat)
  colnames(df) <- cc

  # analyze
  paramstats <- matrix(NA,ncol=n,nrow=2)
  rownames(paramstats) <- c('mean','sd')
  colnames(paramstats) <- cc
  paramstats[1,] <- apply(df,2,mean)
  paramstats[2,] <- apply(df,2,stats::sd)

  # combine
  ret <- rbind(df,paramstats)

  # find parameters with large variation
  if (find.dev) {

    # function to test if any params outside of one standard deviation
    myfunc <- function(xx,u,s) {
      if (any(abs(xx) > u+s*num.sd)) { return(TRUE)}
      return(FALSE)
    }

    test <- vector("logical",n)
    for (i in 1:n) {
      test[i] <- myfunc(ret[(1:N),i],ret[(1+N),i],ret[(2+N),i])
    }

    # find any that tested TRUE
    ind <- which(test)
  }

  if (find.dev) {
    return(list("Ost.data" = ret,"dev.ind"=ind))
  } else {
    return("Ost.data" = ret)
  }

}
