#' @title Write a subbasin group to Raven RVH format
#'
#' @description
#' Writes the subbasins in a given RVH object to Raven RVH format in the specified file.
#'
#' @param rvh rvh object as returned by \code{\link{rvn_rvh_read}} or \code{\link{rvn_rvh_query}}
#' @param sbgroup_name the name of the subbasin group to write to file
#' @param outfile the output file to write the subbasin group to
#' @param subs_per_line the number of subabsins to write per line in the file (default 30)
#' @param overwrite if \code{TRUE}, will overwrite the existing \code{outfile} specified if it already exists (default \code{FALSE})
#'
#' @return Returns \code{TRUE} if the file is written successfully
#'
#' @details
#' Writes the subbasin group for all subbasins in the rvh$SBtable data frame to file using the `:SubbasinGroup`
#' command in Raven. This function is intended to be used with \code{\link{rvn_rvh_query}} to first subset the
#' RVH object in order to create useful groups, though this can be done manually as well.
#'
#' @seealso
#' \code{\link{rvn_rvh_read}} to read a Raven RVH file into R
#' \code{\link{rvn_rvh_query}} to query the RVH file prior to other operations, such as summarizing or writing out subbasin groups)
#'
#' @note
#' Raven has capabilities for creating subbasin and HRU groups that meet certain criteria as well, consider
#' reviewing the `:PopulateSubbasinGroup`, `:PopulateHRUGroup`, and other commands in Section A.3.2 of the
#' Raven User's Manual.
#'
#' @examples
#' # load example rvh file
#' nith <- system.file("extdata","Nith.rvh",package = "RavenR")
#' rvh <- rvn_rvh_read(nith)
#'
#' # query all subbasins upstream of basin 39
#' rvh_upstream_of_39 <- rvn_rvh_query(rvh, subbasinID=39, condition="upstream_of")
#'
#' # temporary filename
#' tf <- file.path(tempdir(), 'mysubbasigroup.rvh')
#'
#' # write this subbasin group to file
#' rvn_rvh_write_subbasingroup(rvh=rvh_upstream_of_39, outfile=tf)
#'
#' @export rvn_rvh_write_subbasingroup
rvn_rvh_write_subbasingroup <- function(rvh=NULL, sbgroup_name=NULL, outfile=NULL, subs_per_line=30, overwrite=TRUE)
{

  # input checking
  if (is.null(rvh) | is.null(rvh$SBtable)) {
    stop("rvn_rvh_summarize: valid rvh object is required")
  }

  if (is.null(sbgroup_name)) {
    sbgroup_name <- "subgroup_RavenR_generated"
  }

  if (is.null(outfile)) {
    outfile <- "subbasingroup_RavenR.rvh"
  }

  fc <- file(outfile, open = "wt")
  writeLines(sprintf(":SubBasinGroup  %s",sbgroup_name), fc)
  for (i in seq(from=1,to=min(nrow(rvh$SBtable),subs_per_line), by=subs_per_line))  {
    temp <- rvh$SBtable$SBID[i:(min(i+subs_per_line-1,nrow(rvh$SBtable)))] %>%
      paste0(collapse=", ")
    sprintf("  %s",temp) %>%
      writeLines(fc)
  }
  writeLines(":EndSubBasinGroup", fc)
  close(fc)

  message(sprintf("Successfully wrote SubBasinGroup '%s' to file %s",sbgroup_name,outfile))
  return(TRUE)
}
