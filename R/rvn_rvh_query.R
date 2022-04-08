#' @title Queries RVH object for subbasins and HRUs of interest
#'
#' @description
#' Queries the RVH object for subbasins or HRUs that are upstream of, downstream of,
#' or the opposite of those conditions, for a given subbasin ID.
#'
#' @param rvh rvh object as returned by \code{\link{rvn_rvh_read}}
#' @param subbasinID subbasinID of basin of interest, as character or integer
#' @param condition condition applied to the query
#'
#' @return
#' {rvh}{rvh object in same format, but queried to condition and all features (SBtable, HRUtable, SBnetwork) updated.}
#'
#' @seealso
#' \code{\link{rvn_rvh_write}} to write contents of the generated (and usually modified HRU and SubBasin tables)
#' \code{\link{rvn_rvh_read}} to read a Raven RVH file into R
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
#' # plot full watershed with igraph library
#' plot(rvh$SBnetwork)
#'
#' # query all subbasins upstream of basin 39, plot
#' rvh_upstream_of_39 <- rvn_rvh_query(rvh, subbasinID=39, condition="upstream_of")
#' plot(rvh_upstream_of_39$SBnetwork)
#'
#' # query of HRUs downstream of basin 39
#' rvn_rvh_query(rvh, subbasinID=39, condition="downstream_of")$SBtable
#'
#' @export rvn_rvh_query
#' @importFrom igraph ego ego_size V as_ids
rvn_rvh_query <- function(rvh=NULL, subbasinID=NULL, condition="upstream_of")
{

  # input checking
  if (is.null(rvh) | is.null(rvh$SBtable)| is.null(rvh$HRUtable) | is.null(rvh$SBnetwork) ) {
    stop("rvn_rvh_query: valid rvh object is required")
  }

  # if direction is downstream, not yet setup - this is upstream only
  if (condition %notin% c("upstream_of","downstream_of","not_upstream_of","not_downstream_of")) {
    stop("rvn_rvh_query: condition must be a character equal to upstream_of,downstream_of,not_upstream_of, or not_downstream_of")
  }

  # check valid subbasinID
  if (as.character(subbasinID) %notin% rvh$SBtable$SBID) {
    stop(sprintf("rvn_rvh_query: subbasinID %s not found in rvh$SBtable, please check the provided subbasinID.",subbasinID))
  }

  # change direction to conditon
  # upstream_of, downstream_of, not_upstream_of, not_downstream_of

  # net <- rvh$SBnetwork
  # SBtable <- rvh$SBtable
  # HRUtable <- rvh$HRUtable

  # out <- SBtable
  egon <- ego(rvh$SBnetwork, order = 100, nodes = V(rvh$SBnetwork), mode = "in") # upstream
  egon_downstream <- ego(rvh$SBnetwork, order = 100, nodes = V(rvh$SBnetwork), mode = "out") # downstream

  # size <- ego_size(net, order = 100, nodes = V(net), mode = "in")
  # count = 1

  ## check subbasinID and assign?
  i <- which(out$SBID == as.character(subbasinID))
  ## SBID <- out[out$SBID == subbasinID,]
  SBID = rvh$SBtable$SBID[i]

  # get upstream subbasins (includes given subbasinID)
  upsubs <- subset.data.frame(rvh$SBtable, SBID %in% as_ids(egon[[i]]))

  # get downstream subbasins
  downsubs <- subset.data.frame(out, SBID %in% as_ids(egon_downstream[[i]]))
  # remove same subbasin from downsubs
  downsubs <- downsubs[downsubs$SBID != SBID,]

  # get not upstream subbasins
  if (condition == "upstream_of") {
    rvh$SBtable <- upsubs
  } else if (condition == "downstream_of") {
    rvh$SBtable <- downsubs
  } else if (condition == "not_upstream_of") {
    rvh$SBtable <- rvh$SBtable[rvh$SBtable$SBID %notin% upsubs$SBID,]
  } else if (condition == "not_downstream_of") {
    rvh$SBtable <- rvh$SBtable[rvh$SBtable$SBID %notint% downsubs$SBID,]
  }

  rvh$HRUtable <- HRUtable[HRUtable$SBID %in% mysubs$SBID,]
  rvh$SBnetwork <- induced_subgraph(rvh$SBnetwork, rvh$SBtable$SBID)

  return(rvh)
}
