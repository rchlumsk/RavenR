#' Generate RVC file from Custom Output CSVs
#'
#' @param filename filepath of rvc file to be created (with .rvc extension)
#' @param custfiles array of filepaths to Raven Custom Output files (must be ByHRU)
#' @param FUN the aggregation function to be applied to state variables (e.g. mean, passed to sapply)
#' @param init_date datetime of model start (optional, can be calculated from Custom Output files)
#' @param ... optional arguments passed to rvn_rvc_write (e.g. author, description)
#'
#' @return TRUE upon success
#' @author Leland Scantlebury
#' @export rvn_rvc_from_custom_output
#'
#' @examples
#' @importFrom zoo index
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
