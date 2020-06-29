#' Obtain set of best params from Ostrich data
#'
#' rvn_ost_bestparams is used to extract the best trials from the Ostrich data,
#' based on the objective function value.
#'
#' rvn_ost_bestparams extracts the runs with the best objective function value from
#' the data supplied. This function may be useful in comparing the parameters
#' of an optimization trial to check for equifinality in models.
#'
#' ost_data is the data object created from rvn_ost_read. If not supplied, this
#' function will look for the OstModel0.txt file in the current directory to
#' read data from.
#'
#' best_num indicates the method used to get the top best trials. If best_num
#' is a double less than 1, this function will return the top percentage of
#' best trials. If best_num is an integer, the function will return the top X
#' trials in the file, where X=best_num.
#'
#' num_metrics refers to the number of fit metrics used in the Ostrich
#' calibration, of which there should be at least one. If this value is
#' supplied, the function will remove the metric columns from the returned
#' data.
#'
#' remove_run is a boolean to indicate whether the run column should be
#' removed.
#'
#' @param ost_data data object from rvn_ost_read function (optional)
#' @param best_num indicates the method for selecting top best runs
#' @param num_metrics number of fit metrics used in Ostrich; will remove the
#' metrics if supplied
#' @param remove_run boolean for removing the 'Run' column in returned data
#' @return \item{ost_bestruns}{Ostrich trials with the top best objective
#' function}
#' @seealso \code{\link{rvn_ost_read}} for reading in Ostrich files
#'
#' \code{\link{rvn_ost_plot}} for creating various plots with Ostrich data
#'
#' \code{\link{ost_params_compare}} for comparing parameters between multiple
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
#' mydata <- rvn_ost_read(ff)
#'
#' # top 30 percent of runs, no removal of metrics
#' mybest <- rvn_ost_bestparams(mydata,best_num=0.3)
#' head(mybest)
#'
#' # minimalist function run (requires OstModel0.txt in current directory)
#' mybest <- rvn_ost_bestparams(mydata)
#' head(mybest)
#' }
#'
#' @export rvn_ost_bestparams
rvn_ost_bestparams <- function(ost_data=NA,best_num=0.1,num_metrics=NA,remove_run=T) {

  # get Ostrich data
  if (is.null(nrow(ost_data))) {
    if (file.exists('OstModel0.txt')) {
      warning("Using OstModel0.txt in current folder, ost_data not supplied")
      ost_data <- read.table('OstModel0.txt',header=T)
    } else {
      stop("ost_data is required and no OstModel0.txt file found in current folder")
    }
  }

  # basic checks
  if (num_metrics < 1 & !(is.na(floor(num_metrics)))) { stop("Cannot have less than 1 metric; check num_metrics.")}
  N <- nrow(ost_data)
  if (best_num > nrow(ost_data)) {stop("best_num is greater than the number of calibration trials")}
  if (best_num < 0) { stop("best_num must be positive")}


  # get best trials
  if (!(is.na(floor(num_metrics)))) {
    ost_data <- ost_data[,-(3:(floor(num_metrics)+2))] # remove metrics aside from obj function
    ost_data.sorted <- ost_data[order(ost_data$obj.function),]
  } else {
    ost_data.sorted <- ost_data[order(ost_data$obj.function),]
  }

  if (remove_run) {
    ost_data.sorted <- ost_data.sorted[,-1]
  }

  if (best_num < 1) {
    best.trials <- ost_data.sorted[1:(floor(N*best_num)),]
  } else {
    best.trials <- ost_data.sorted[1:floor(best_num),]
  }

  return("ost_bestruns" = best.trials)
}
