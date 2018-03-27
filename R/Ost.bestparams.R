#' Obtain set of best params from Ostrich data
#'
#' Ost.bestparams is used to extract the best trials from the Ostrich data,
#' based on the objective function value.
#'
#' Ost.bestparams extracts the runs with the best objective function value from
#' the data supplied. This function may be useful in comparing the parameters
#' of an optimization trial to check for equifinality in models.
#'
#' Ost.data is the data object created from Ost.read. If not supplied, this
#' function will look for the OstModel0.txt file in the current directory to
#' read data from.
#'
#' best.num indicates the method used to get the top best trials. If best.num
#' is a double less than 1, this function will return the top percentage of
#' best trials. If best.num is an integer, the function will return the top X
#' trials in the file, where X=best.num.
#'
#' num.metrics refers to the number of fit metrics used in the Ostrich
#' calibration, of which there should be at least one. If this value is
#' supplied, the function will remove the metric columns from the returned
#' data.
#'
#' remove.run is a boolean to indicate whether the run column should be
#' removed.
#'
#' @param Ost.data data object from Ost.read function (optional)
#' @param best.num indicates the method for selecting top best runs
#' @param num.metrics number of fit metrics used in Ostrich; will remove the
#' metrics if supplied
#' @param remove.run boolean for removing the 'Run' column in returned data
#' @return \item{Ost.bestruns}{Ostrich trials with the top best objective
#' function}
#' @seealso \code{\link{Ost.read}} for reading in Ostrich files
#'
#' \code{\link{Ost.plot}} for creating various plots with Ostrich data
#'
#' \code{\link{Ost.params.compare}} for comparing parameters between multiple
#' Ostrich calibration jobs
#'
#' Download Ostrich at
#' \href{http://www.eng.buffalo.edu/~lsmatott/Ostrich/OstrichMain.html}{Shawn
#' Matott's web page}
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for other software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Ostrich calibration parameter best
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#'
#' # locate sample OstModel0.txt file in RavenR package
#' ff <- system.file("extdata","OstModel0.txt", package="RavenR")
#'
#' # read in file
#' mydata <- Ost.read(ff)
#'
#' # top 30 percent of runs, no removal of metrics
#' mybest <- Ost.bestparams(mydata,best.num=0.3)
#' head(mybest)
#'
#' # minimalist function run (requires OstModel0.txt in current directory)
#' mybest <- Ost.bestparams(mydata)
#' head(mybest)
#' }
#'
#' @export Ost.bestparams
Ost.bestparams <- function(Ost.data=NA,best.num=0.1,num.metrics=NA,remove.run=T) {

  # get Ostrich data
  if (is.null(nrow(Ost.data))) {
    if (file.exists('OstModel0.txt')) {
      warning("Using OstModel0.txt in current folder, Ost.data not supplied")
      Ost.data <- read.table('OstModel0.txt',header=T)
    } else {
      stop("Ost.data is required and no OstModel0.txt file found in current folder")
    }
  }

  # basic checks
  if (num.metrics < 1 & !(is.na(floor(num.metrics)))) { stop("Cannot have less than 1 metric; check num.metrics.")}
  N <- nrow(Ost.data)
  if (best.num > nrow(Ost.data)) {stop("best.num is greater than the number of calibration trials")}
  if (best.num < 0) { stop("best.num must be positive")}


  # get best trials
  if (!(is.na(floor(num.metrics)))) {
    Ost.data <- Ost.data[,-(3:(floor(num.metrics)+2))] # remove metrics aside from obj function
    Ost.data.sorted <- Ost.data[order(Ost.data$obj.function),]
  } else {
    Ost.data.sorted <- Ost.data[order(Ost.data$obj.function),]
  }

  if (remove.run) {
    Ost.data.sorted <- Ost.data.sorted[,-1]
  }

  if (best.num < 1) {
    best.trials <- Ost.data.sorted[1:(floor(N*best.num)),]
  } else {
    best.trials <- Ost.data.sorted[1:floor(best.num),]
  }

  return("Ost.bestruns" = best.trials)
}
