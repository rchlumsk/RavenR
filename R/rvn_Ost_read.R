#' Read Data from Ostrich output
#'
#' rvn_ost_read is used to read in Ostrich output data and return it as a data
#' object. This function is a wrapper for the read.table function.
#'
#' @param ff full file path to the OstModel0.txt file; default 'OstModel0.txt'
#' @return \item{ost_dd}{Ostrich data used in plotting}
#' @seealso \code{\link{rvn_ost_plot}} for handling Raven hydrograph files
#' # warning: example not run, sample example for associated files only
#' Download Ostrich at
#' \href{http://www.eng.buffalo.edu/~lsmatott/Ostrich/OstrichMain.html}{Shawn
#' Matott's web page}
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for other software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Ostrich calibration
#' @examples
#'
#' # locate sample OstModel0.txt file in RavenR package
#' ff <- system.file("extdata","OstModel0.txt", package="RavenR")
#'
#' # read in file
#' mydata <- rvn_ost_read(ff)
#'
#' # basic plot of parameter par_g_3 through iterations
#' plot(mydata$par_g_3)
#'
#' @export rvn_ost_read
rvn_ost_read <- function(ff='OstModel0.txt') {
  if(file.exists(ff)) {
    ost_dd <- read.table(ff,header=T)
  } else { stop(sprintf("Cannot find file %s :( Check your current directory and file name.",ff))}
  return(ost_dd)
}
