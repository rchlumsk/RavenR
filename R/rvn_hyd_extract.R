#' @title Extract function for Raven Hydrograph object
#'
#' @description
#' rvn_hyd_extract is used for extracting data from the Raven hydrograph object.
#' Works for objects passed from rvn_hyd_read function (which reads the Hydrographs.csv file produced by the modelling framework Raven).
#'
#' @details
#' rvn_hyd_extract is used to extract the modelled and observed data from a Raven
#' hydrograph object by name reference. It is also easy to create plots of
#' modelled and observed data using this function. The simulated and observed
#' files are outputted regardless of whether a plot is created, for the
#' specified period.
#'
#' The subs input is the name of the column desired for use; the most common
#' use of this will be for subbasin outflows, where the names will be of the
#' form "subXX", for example "sub24".
#'
#' The hyd object is the full hydrograph object (hyd and units in one data
#' frame) created by the hyd.read function. Both the hyd and units are
#' required, since the units are placed onto the plots if one is created. This
#' is useful to at least see the units of the plotted variable, even if the
#' plot is later modified.
#'
#' The prd input is used to specify a period for the plot and/or the data
#' output. The period should be specified as a string start and end date, of
#' the format "YYYY-MM-DD/YYYY-MM-DD", for example, "2006-10-01/2010-10-01". If
#' no period is supplied, the entire time series will be used.
#'
#' @param subs column name for plotting/extracting
#' @param hyd full hydrograph data frame (including units) produced by hyd.read
#' @param prd time period for plotting, as string. See details
#' @return returns a list with sim, obs, and inflow time series
#'  \item{sim}{model simulation for specified column and period}
#'  \item{obs}{observed data for specified column and period}
#'  \item{inflow}{inflow simulation for specified column and period}
#'
#' @seealso \code{\link{rvn_hyd_read}} for reading in the Hydrographs.csv file and
#' creating the object required in this function.
#' \code{\link{rvn_hyd_plot}} for conveniently plotting the output object contents onto the same figure.
#'
#' @examples
#' # read in hydrograph sample csv data from RavenR package
#' ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")
#'
#' # read in Raven Hydrographs file, store into myhyd
#' myhyd <- rvn_hyd_read(ff)
#'
#' # no plot or observed data, specified period
#' flow_36 <- rvn_hyd_extract(subs="Sub36",myhyd)
#'
#' attributes(flow_36)
#'
#' # extract simulated and observed flows
#' sim <- flow_36$sim
#' obs <- flow_36$obs
#'
#' # extract precipitation forcings
#' myprecip <- rvn_hyd_extract(subs="precip",hyd=myhyd)
#' myprecip <- myprecip$sim
#'
#' # plot all components using rvn_hyd_plot
#' rvn_hyd_plot(sim,obs,precip=myprecip)
#'
#' @export rvn_hyd_extract
rvn_hyd_extract <- function(subs=NA, hyd=NA, prd=NULL) {

  if (missing(subs)) {
    stop("subs is required for this function.")
  }
  if (missing(hyd)) {
    stop("hyd is required for this function; please supply the full output file from hyd.read.")
  }

  mysub <- NULL

  # extract random pair
  hydrographs <- hyd$hyd
  units <- hyd$units
  mycols <- colnames(hydrographs)
  subID <- gsub("[^0-9]", "", subs)
  mysub.sim <- sprintf("\\b%s\\b",subs)
  mysub.obs <- sprintf("\\b%s_obs\\b",subs)
  mysub.inflow <- sprintf("\\b%s_resinflow\\b",subs)

  ind.sim <- grep(mysub.sim,mycols)
  ind.obs <- grep(mysub.obs,mycols)
  ind.inflow <- grep(mysub.inflow,mycols)

  ind <- c(ind.sim,ind.inflow,ind.obs)

  if (length(ind)==0) {
    stop(sprintf("%s not found in the columns, check the supplied subs argument.",mysub))
  } else if (length(ind) > 3) {
    stop(sprintf("There are %i matches for %s, expect a maximum of 3.",length(ind),mysub))
  }

  # assume first column is always simulated one; observed or inflows follow afterwards
  # assume if all 3 columns exist, observed is the second one (obs before inflows)
  mysim <- NULL
  myobs <- NULL
  myinflow <- NULL

  if (length(ind.sim) == 1) {
    mysim <- hydrographs[,ind.sim]
  }
  if (length(ind.obs) == 1) {
    myobs <- hydrographs[,ind.obs]
  }
  if (length(ind.inflow) == 1) {
    myinflow <- hydrographs[,ind.inflow]
  }

  # determine the period to use
  # if (!(is.null(prd))) {
  #
  #   # prd is supplied; check that it makes sense
  #   firstsplit <- unlist(strsplit(prd,"/"))
  #   if (length(firstsplit) != 2) {
  #     stop("Check the format of supplied period argument prd; should be two dates separated by '/'.")
  #   }
  #   if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
  #       || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
  #     stop("Check the format of supplied period argument prd; two dates should be in YYYY-MM-DD format.")
  #   }
  #   # add conversion to date with xts format check ?
  #
  # } else {
  #   # period is not supplied
  #
  #   # not using smart.period function and no period supplied; use whole range
  #   N <- nrow(hydrographs)
  #   prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(hydrographs[1,1]),month(hydrographs[1,1]),day(hydrographs[1,1]),
  #                     year(hydrographs[N,1]),month(hydrographs[N,1]),day(hydrographs[N,1]) )
  # }
  prd <- rvn_get_prd(mysim, prd)

  # return values
  return(list("sim" = mysim[prd,1], "obs" = myobs[prd,1],"resinflow"=myinflow[prd,1]))
}

