#' Plot data from Ostrich output
#'
#' Ost.plot is used to plot data from calibrations performed using Ostrich.
#'
#' The Ost.plot is used to create various types of plots using the data from an
#' Ostrich calibration run. For simplicity only the 'OstModel0.txt' file or
#' data is used; this can be passed as a data object from Ost.read, or if NULL
#' the function will look for the OstModel0.txt file in the current directory
#' and use that.
#'
#' The plot.type can be either 'oft', 'boft', or 'psa', which refers to
#' objective function vs trials, best objective function vs trials, and
#' parameter sensitivity analysis for a single parameter plot.
#'
#' logy is used to plot the y axis logarithmically; note that this only applies
#' for the plot.type oft or boft, not for the psa plots.
#'
#' rangey only applies to the oft plot type.
#'
#' num.metrics refers to the number of fit metrics used in the Ostrich
#' calibration; note that there should be at least one fit metric used in each
#' calibration run.
#'
#' param.index refers to the index of the parameter you wish to plot for the
#' psa plot type. Note that the index is set for the parameters specifically,
#' not the column index in the OstModel0.txt file. For example, setting
#' param.index = 1 will create a plot of the first parameter in the
#' OstModel0.txt file.
#'
#' @param Ost.data data object from Ost.read function (optional)
#' @param plot.type the type of plot to generate
#' @param return.data whether to return the Ostrich data used in plotting
#' @param logy boolean to plot the y axis on a log scale
#' @param rangey range of the y axis; optional
#' @param num.metrics number of fit metrics used in Ostrich
#' @param param.index index of parameter to plot for parameter sensitivity
#' plots
#' @return \item{Ost.dd}{Ostrich data used in plotting; returned only if
#' return.data is TRUE. Else function returns TRUE if the function executed
#' properly}
#' @seealso \code{\link{hyd.read}} for handling Raven hydrograph files
#'
#' Download Ostrich at
#' \href{http://www.eng.buffalo.edu/~lsmatott/Ostrich/OstrichMain.html}{Shawn
#' Matott's web page}
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for other software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Ostrich calibration
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
#' # simple plotting of the oft plot type
#' Ost.plot(Ost.data=mydata)
#'
#' # plot of the best objective function vs trials
#' Ost.plot(mydata,plot.type='boft')
#'
#' # sensitivty analysis of the 5th parameter
#' Ost.plot(mydata,plot.type='psa',num.metrics=4,param.index=5)
#' }
#'
#' @export Ost.plot
Ost.plot <- function(Ost.data=NA,plot.type='oft',return.data=F,logy=F,rangey=NA,num.metrics=1,param.index=1) {

  # get Ostrich data
  if (is.null(nrow(Ost.data))) {
    if (file.exists('OstModel0.txt')) {
      warning("Using OstModel0.txt in current folder, Ost.data not supplied")
      Ost.data <- utils::read.table('OstModel0.txt',header=T)
    } else {
      stop("Ost.data is required and no OstModel0.txt file found in current folder")
    }
  }

  # basic checks
  if (num.metrics < 1) { stop("Cannot have less than 1 metric; check num.metrics.")}
  if (param.index < 1) { stop("Cannot have less than 1 parameter; check param.index")}
  if (!(is.na(rangey)) && length(rangey) != 2) {stop("rangey must be of length 2, if supplied.")}

  if (plot.type == 'oft') {

    if (logy) {
      if (is.na(rangey)) {
        plot(Ost.data$Run,Ost.data$obj.function,
             xlab='Trial',ylab='Objective Function',
             log="y")
      } else {
        if (rangey[1] <= 0) {stop("Y range not valid for a log range")}
        plot(Ost.data$Run,Ost.data$obj.function,
             xlab='Trial',ylab='Objective Function',
             log="y", ylim=rangey)
      }

    } else {
      if (is.na(rangey[1])) {
        plot(Ost.data$Run,Ost.data$obj.function,
             xlab='Trial',ylab='Objective Function')
      } else {
        plot(Ost.data$Run,Ost.data$obj.function,
             xlab='Trial',ylab='Objective Function',
             ylim=rangey)
      }
      title('Objective Function v Trial',cex.main=1)
    }
  } else if (plot.type == 'boft') {
    ### create plot of best obj function vs trial

    # need to evaluate the best of each trial
    best.obj <- rep(NA,nrow(Ost.data))
    best.obj[1] <- Ost.data$obj.function[1]
    best <- best.obj[1]
    for (i in 2:nrow(Ost.data)) {
      if (Ost.data$obj.function[i] < best) {
        best = Ost.data$obj.function[i]
      }
      best.obj[i] = best
    }

    # create plot of best obj function vs trial
    if (logy) {
      plot(Ost.data$Run,best.obj,ylim=rev(range(best.obj)),
           xlab='Trial',ylab='Best Objective Function',
           type='l',log='y')
    } else {
      plot(Ost.data$Run,best.obj,ylim=rev(range(best.obj)),
           xlab='Trial',ylab='Best Objective Function',
           type='l')
    }
    title('Best Objective Function v Trial',cex.main=1)

  } else if (plot.type == 'psa') {
    ### create SA plot for a given parameter
    cols <- colnames(Ost.data)
    col.num <- 3 + num.metrics + param.index # reference for parameter column

    plot(Ost.data[,col.num],Ost.data$obj.function,
         ylab='Objective Function',xlab=cols[col.num],
         main='Marginal Parameter Sensitivty',cex.main=1)
  } else {
    stop("Plot type not recognized.")
  }

  # supplement data with the best obj vector
  if (return.data) {
    Ost.data <- data.frame(Ost.data,best.obj)
  } else {
    return(TRUE)
  }

}
