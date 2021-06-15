#' @title Generate RVC file from Custom Output CSVs
#'
#' @description
#' rvn_rvc_from_custom_output creates an initial conditions rvc file that specifies the initial state
#' using the information from the supplied custom output file.
#'
#' @param filename filepath of rvc file to be created (with .rvc extension)
#' @param custfiles array of filepaths to Raven Custom Output files (must be ByHRU)
#' @param FUN the aggregation function to be applied to state variables (e.g. mean, passed to sapply)
#' @param init_date datetime of model start (optional, can be calculated from Custom Output files)
#' @param ... optional arguments passed to rvn_rvc_write (e.g. author, description)
#'
#' @return TRUE upon success
#' @author Leland Scantlebury
#'
#' @examples
#' # Create array of custom output file(s)
#' custout <- c(system.file("extdata","run1_SNOW_Daily_Average_ByHRU.csv",package = "RavenR"))
#'
#' # Create rvc file of mean snow for each HRU
#' rvn_rvc_from_custom_output(filename = file.path(tempdir(), "Blank.rvc"),
#'                            custfiles = custout,
#'                            FUN = mean)
#' @importFrom zoo index
#' @export rvn_rvc_from_custom_output
rvn_rvc_from_custom_output <- function(filename, custfiles, FUN, init_date=NULL,...){

  #-- ValiDate init_date
  if (!is.null(init_date)){
    if(as.Date(init_date)!=init_date){
      stop("init_date is not a valid datetime object")
    }
  }

  #-- Initialize HRU initial conditions dataframe
  initHRU <- NULL

  #-- Process output files
  for (outfile in custfiles) {
    cust <- rvn_custom_read(outfile)
    svname <- attr(cust, 'datatype')

    #-- Ensure ByHRU Custom Output
    #TODO Support other outputs
    if (attr(cust, 'space_agg') != 'ByHRU') {
      stop('Only ByHRU Custom Output files are supported at this time.')
    }

    #-- Make df if doesn't exist yet
    if (is.null(initHRU)) {
      initHRU <- data.frame('HRU'=1:attr(cust,'HRUs'))
    }

    #-- Summarize by column (HRU) using FUN
    agg_byHRU <- sapply(cust, FUN=FUN)

    #-- Add to SV by HRU df
    initHRU[,svname] <- agg_byHRU
  }

  #-- If init_time wasn't passed, use the minimum from the custom output
  if(is.null(init_date)) {
    init_date <- min(zoo::index(cust))
  }

  #-- Write file
  rvn_rvc_write(filename = filename,initHRU = initHRU,init_date = init_date,...)

  #-- Done
  return(TRUE)
}
