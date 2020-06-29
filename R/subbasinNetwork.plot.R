#' Plot Raven subbasin network.
#'
#' @description
#' Takes the information gathered from an .rvh file via the function \code{\link{rvh.read}} and generates a plot of the subbasin network,
#' where nodes are located at SubBasin lat-long centroids, and edge widths of the network correspond to contributing upstream area.
#'
#' @param SBtable a valid table of Raven subbasins, obtained from \code{\link{rvh.read}}
#' @param labeled TRUE if the nodes are labeled with the SubBasin ID, SBID
#'
#' @return TRUE if completed
#'
#' @author James R. Craig, University of Waterloo
#'
#' @seealso
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#' \code{\link{rvh.read}} to read in the subbasin data table
#'
#' @examples
#'  # warning: example not run in compiling package
#' \dontrun{
#'  # read in rvh file
#'  rvh<-rvh.read("example.rvh")
#'
#'  # create network plot of watershed structure from rvh file
#'  subbasinNetwork.plot(rvh$SBtable)
#' }
#'
#' @keywords Raven Network Stream Plot
#'
#' @export subbasinNetwork.plot

subbasinNetwork.plot<-function(SBtable,labeled=FALSE)
{
  links<-data.frame(SBID=SBtable$SBID,downID=SBtable$Downstream_ID)
  links<-subset.data.frame(links,downID>=0) # get rid of -1

  #create network graph structure
  net <- igraph::graph_from_data_frame(d=links, vertices=SBtable, directed=T)

  #set subbasin node parameters
  #igraph::V(net)$size<-20*(SBtable$Area/mean(SBtable$Area))
  #igraph::V(net)$label.dist=2*(SBtable$Area/mean(SBtable$Area))^0.6
  igraph::V(net)$size<-0
  igraph::V(net)$label.dist=1
  igraph::V(net)$color="darkgreen"
  igraph::V(net)$frame.color=NA
  igraph::V(net)$label.cex=0.75
  igraph::V(net)$label.family="sans"
  igraph::V(net)$label.color="black"
  igraph::V(net)$label.degree=pi/4

  if (labeled==FALSE){igraph::V(net)$label<-NA}

  igraph::E(net)$color="blue"
  #E(net)$curved="true"
  coords=data.frame(long=SBtable$AvgLongit,lat=SBtable$AvgLatit) #long=x, lat=y

  vv<-igraph::head_of(net,igraph::E(net))

  igraph::E(net)$width=20*((SBtable$TotalUpstreamArea[vv]+SBtable$Area[vv])/(max(SBtable$TotalUpstreamArea+SBtable$Area)))^0.8

  plot(net,layout=data.matrix(coords),edge.arrow.size=0.0)

  return (TRUE)
}
