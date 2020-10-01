#' @title Plot Raven subbasin network.
#'
#' @description
#' Generates a plot of the subbasin network from rvh file information.
#'
#' @details
#' Takes the information gathered from an .rvh file via the function \code{\link{rvn_rvh_read}} and
#' generates a plot object of the subbasin network,
#' where nodes are located at SubBasin lat-long centroids, and
#' edge widths of the network correspond to contributing upstream area.
#'
#' @param SBtable a valid table of Raven subbasins, obtained from \code{\link{rvn_rvh_read}}
#' @param labeled TRUE if the nodes are labeled with the SubBasin ID, SBID
#'
#' @return {p1}{ggplot object of subbasin network plot}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @seealso
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#' \code{\link{rvn_subbasin_map}} to map results for subbasins.
#'
#' @examples
#' # read in rvh file
#' rvh <- rvn_rvh_read(system.file("extdata","Nith.rvh", package="RavenR"))
#'
#' # create network plot of watershed structure from rvh file
#' rvn_subbasin_network_plot(rvh$SBtable)
#'
#' # include labels
#' rvn_subbasin_network_plot(rvh$SBtable, labeled=T)
#'
#' @keywords Raven Network Stream Plot
#'
#' @export rvn_subbasin_network_plot
#' @importFrom ggplot2 ggplot geom_segment geom_point theme aes geom_text geom_label
#' @importFrom igraph graph_from_data_frame get.data.frame
#' @importFrom colorspace coords
rvn_subbasin_network_plot <- function(SBtable,labeled=FALSE)
{

  links<-data.frame(SBID=SBtable$SBID,downID=SBtable$Downstream_ID)
  links<-subset.data.frame(links,downID>=0) # get rid of -1

  #create network graph structure
  net <- igraph::graph_from_data_frame(d=links, vertices=SBtable, directed=T)

  # Get subbasin coordinates
  coords=data.frame(long=SBtable$AvgLongit,lat=SBtable$AvgLatit, SB = SBtable$SBID)

  # Get up and downstream links
  g <- igraph::get.data.frame(net)
  g$from.x <- coords$long[match(g$from,coords$SB)]
  g$from.y <- coords$lat[match(g$from,coords$SB)]
  g$to.x <- coords$long[match(g$to,coords$SB)]
  g$to.y <- coords$lat[match(g$to,coords$SB)]

  # Calculate Width based on upstream/total area
  g$TotalUpstreamArea <- SBtable$TotalUpstreamArea[match(g$from, SBtable$SBID)]
  g$Area <- SBtable$Area[match(g$from, SBtable$SBID)]
  g$width <- ((g$TotalUpstreamArea+g$Area)/(max(SBtable$TotalUpstreamArea+SBtable$Area)))^0.8

  #Create Plot
  p1 <- ggplot()+
    geom_segment(data=g,aes(x=from.x,xend = to.x, y=from.y,yend = to.y, size = width), colour="blue") +
    geom_point(data = coords, aes(x = long, y = lat), color = "darkgreen")+
    rvn_theme_RavenR()+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          legend.position = "none")

  if (labeled == TRUE) {
    p1 <- p1 +
      geom_label(data = coords, aes(x = long, y= lat, label = SB), hjust = 0, nudge_x = 0.001)
  }

  return(p1)
}
