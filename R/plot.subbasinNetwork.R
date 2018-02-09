#' Plot Subbasin Network
#'
#' plot.subbasinNetwork creates a network plot of the subbasins using the igraph package
#'
#' Plots subbasin network connectivity as specified in a Raven .rvh file. Centroids of
#' basins in lat-long are used to orient the network, so it is geographically consistent.
#' Areas are proportional to subbasin total area. Labels refer to subbasin ID.
#'
#' Depends: igraph library.
#'
#' Function contributed by Dr. James Craig
#'
#' @param SBnetwork SBnetwork output from rvh.read function
#' @return \item{net}{igraph net object of watershed}
#'
#' @seealso \code{\link{rvh.read}} for reading in rvh files
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven rvh subbasin watershed HRU network plot igraph
#' @examples
#'
#' # Using a valid Raven model .rvh file in the working directory,
#' # read the file into the rvh object
#' rvh<-rvh.read("WatershedX.rvh")
#' plot
#' plot.subbasinNetwork(rvh$SBtable)e)
#'
#' @export plot.subbasinNetwork
plot.subbasinNetwork<-function(SBtable)
{
  library('igraph')
  links<-data.frame(SBID=SBtable$SBID,downID=SBtable$Downstream_ID)
  links<-subset.data.frame(links,downID>=0) # get rid of -1

  #create network graph structure
  net <- graph_from_data_frame(d=links, vertices=SBtable, directed=T)

  #calculate upstream area for subbasins
  # V(net)$shape<-"square"
  V(net)$size<-20*(SBtable$Area/mean(SBtable$Area))
  #V(net)$size<-2*sqrt(SBtable$TotalUpstreamArea)
  V(net)$label.cex=0.75
  V(net)$label.dist=2*(SBtable$Area/mean(SBtable$Area))^0.6
  V(net)$color="darkgreen"
  V(net)$frame.color=NA

  V(net)$size<-0
  V(net)$label.dist=1

  V(net)$label.family="sans"
  V(net)$label.color="black"
  V(net)$label.degree=pi/4
  E(net)$color="blue"
  V(net)$label<-NA
  #V(net)$color=NA
  #E(net)$curved="true"
  coords=data.frame(long=SBtable$AvgLongit,lat=SBtable$AvgLatit)

  vv<-tail_of(net,E(net))
  vv

  E(net)$width=17*(SBtable$TotalUpstreamArea[vv]/(max(SBtable$TotalUpstreamArea)))^0.8

  plot(net,layout=data.matrix(coords),edge.arrow.size=0.0)

  return (net)
}
