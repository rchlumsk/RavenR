#' @title Extract function for Raven Reservoir object
#'
#' @description
#' rvn_res_extract is used for extracting data from the Raven reservoir object.
#' Works for objects from \code{\link{rvn_res_read}} function (for reading in the ReservoirStages.csv
#' file).
#'
#' @details
#' rvn_res_extract is used to extract the modelled and observed data from a Raven
#' reservoir object by name reference. It is also easy to create plots of
#' modelled and observed data using this function. The simulated and observed
#' files are outputted regardless of whether a plot is created, for the
#' specified period.
#'
#' The subs input is the name of the column desired for use; the most common
#' use of this will be for subbasins, where the names will be of the form
#' "subXX", for example "sub24".
#'
#' The res object is the full reservoir object (res and units in one data
#' frame) created by the res.read function. Both the res and units are
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
#' @param res full reservoir data frame (including units) produced by res.read
#' @param prd time period for plotting, as string. See details
#' @return \item{sim}{model simulation for specified column and period}
#' \item{obs}{observed data for specified column and period}
#' \item{inflow}{inflow simulation for specified column and period}
#'
#' @seealso \code{\link{rvn_res_read}} for reading in the Reservoirs.csv file and
#' creating the object required in this function. \code{\link{rvn_res_plot}} for
#' plotting the extracted stage time series
#'
#' @examples
#'
#' ff <- system.file("extdata","ReservoirStages.csv", package="RavenR")
#'
#' # Read in Raven Reservoirs file, store into myres
#' myres <- rvn_res_read(ff)
#'
#' # Extract stage using this function
#' stage36 <- rvn_res_extract(subs="sub36",res=myres,prd="2002-10-01/2003-10-01")
#' summary(stage36)
#' summary(stage36$sim)
#'
#' # Example for precipitation
#' precip <- rvn_res_extract(subs="precip",res=myres)
#'
#' @export rvn_res_extract
rvn_res_extract <- function(subs=NA, res=NA, prd=NULL) {

  if (missing(subs)) {
    stop("subs is required for this function.")
  }
  if (missing(res)) {
    stop("res is required for this function; please supply the full output file from res.read.")
  }

  mysub <- NULL

  # extract random pair
  reservoirs <- res$res
  units <- res$units
  mycols <- colnames(reservoirs)
  subID <- gsub("[^0-9]", "", subs)

  mysub.sim <- sprintf("\\b%s\\b",subs) # sim does not have a sim
  mysub.obs <- sprintf("\\b%s_obs\\b",subs)
  mysub.inflow <- sprintf("\\b%s_inflow\\b",subs)
  # ind.base <- grep(mysub,mycols)
  # ind.sim <- ind.base[1]  # assume sim is always there and first
  # ind.inflow <- ind.base[grep(mysub.inflow,mycols[ind.base])]
  # ind.obs <- ind.base[grep(mysub.obs,mycols[ind.base])]

  ind.sim <- grep(mysub.sim,mycols)
  ind.obs <- grep(mysub.obs,mycols)
  ind.inflow <- grep(mysub.inflow,mycols)

  ind <- c(ind.sim,ind.inflow,ind.obs)

  # mysub <- subs # sprintf("\\b%s\\b",subs)
  # mysub.obs <- "obs"
  # mysub.inflow <- "inflow"
  # ind.base <- grep(mysub,mycols)
  # ind.sim <- ind.base[1]  # assume sim is always there and first
  # ind.inflow <- ind.base[grep(mysub.inflow,mycols[ind.base])]
  # ind.obs <- ind.base[grep(mysub.obs,mycols[ind.base])]
  # ind <- c(ind.sim,ind.inflow,ind.obs)

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
    mysim <- reservoirs[,ind.sim]
  }
  if (length(ind.obs) == 1) {
    myobs <- reservoirs[,ind.obs]
  }
  if (length(ind.inflow) == 1) {
    myinflow <- reservoirs[,ind.inflow]
  }

  # # determine the period to use
  # if (!(is.null(prd))) {
  #
  #   # period is supplied; check that it makes sense
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
  #   N <- nrow(reservoirs)
  #   prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(reservoirs[1,1]),month(reservoirs[1,1]),day(reservoirs[1,1]),
  #                     year(reservoirs[N,1]),month(reservoirs[N,1]),day(reservoirs[N,1]) )
  # }

  prd <- rvn_get_prd(mysim, prd)

  # return values
  return(list("sim" = mysim[prd,1], "obs" = myobs[prd,1],"inflow"=myinflow[prd,1]))
}

