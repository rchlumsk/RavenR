#' Plot Raven hydrologic process network
#'
#' This routine takes a connections datafrom generated using rvi.connections()
#' and plots the connections information as a network graph.
#'
#' @param connections a dataframe of from-to connections generated using rvi.connections()
#' @param pdfout name of pdf file to save the network plot to
#'
#' @return {TRUE}{returns TRUE if executed properly. Also generates a pdf plot in working directory.}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @note tries to follow basic network structure, accomodates unrecognized state variables on LHS of plot
#'
#' @seealso  rvi.connections rvi.read
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#'  \dontrun{
#'   # sample workflow of rvh.read
#'
#'   rvi<-rvi.read("example.rvi")
#'
#'   # get number of Hydrologic processes
#'   numProcss<-nrow(rvi$HydProcTable)
#'}
#' @keywords Raven  rvi  Hydrologic Processes
#' @export rvn_rvi_hpnet_plot
rvn_rvi_hpnet_plot<-function(connections,pdfout="network.pdf") {
  require(igraph)
  source<-connections$From
  target<-connections$To
  process<-connections$ProcessType
  nodes<-data.frame(source,target,process)
  network<-graph_from_data_frame(d=nodes,directed=T)

  linetype<-rep(1,nrow(connections))
  linetype[!is.na(connections$Conditional)]=2

  linecol<-rep("black",nrow(connections))
  linecol[!is.na(connections$Conditional)]="blue"

  verts<-vertex.attributes(network,)$name
  nverts<-length(verts)
  layout<-matrix(1:nverts*2,nrow=nverts,ncol=2)
  count=1
  for (i in 1:nverts)
  {
    if      (verts[i]=="ATMOSPHERE"){layout[i,1]=5; layout[i,2]=6;}
    else if (verts[i]=="ATMOS_PRECIP"){layout[i,1]=1; layout[i,2]=6.2;}

    else if (verts[i]=="CANOPY_SNOW"){layout[i,1]=0; layout[i,2]=5;}
    else if (verts[i]=="CANOPY"     ){layout[i,1]=1; layout[i,2]=5.3;}

    else if (verts[i]=="SNOW_LIQ"         ){layout[i,1]=-1; layout[i,2]=4;}
    else if (verts[i]=="SNOW"         ){layout[i,1]=0; layout[i,2]=4.3;}
    else if (verts[i]=="PONDED_WATER" ){layout[i,1]=1; layout[i,2]=4.6;}
    else if (verts[i]=="DEPRESSION" ){layout[i,1]=2; layout[i,2]=4.9;}
    else if (verts[i]=="WETLAND" ){layout[i,1]=3; layout[i,2]=5.2;}

    else if (verts[i]=="SOIL[0]"    ){layout[i,1]=2; layout[i,2]=3;}
    else if (verts[i]=="SURFACE_WATER"    ){layout[i,1]=6; layout[i,2]=3;}

    else if (verts[i]=="SOIL[1]"    ){layout[i,1]=2; layout[i,2]=2;}
    else if (verts[i]=="FAST_RESERVOIR"    ){layout[i,1]=2; layout[i,2]=2;}

    else if (verts[i]=="SOIL[2]"    ){layout[i,1]=2; layout[i,2]=1;}
    else if (verts[i]=="SLOW_RESERVOIR"){layout[i,1]=2; layout[i,2]=1;
    }
    else { layout[i,2]=count; count=count+1;layout[i,1]=-2;}
  }

  #print(layout)
  pdf(pdfout)
  plot.igraph(network,
              layout=layout,#layout_nicely(network),
              edge.color=linecol,
              edge.arrow.size=0.5,
              #            edge.label = nodes$process,
              edge.label.family="sans",
              edge.label.cex=0.4,
              edge.lty=linetype,
              #edge.curved=T,
              frame.color="darkslategray2",
              vertex.label.family="sans",
              vertex.frame.color=NA,
              vertex.shape="rectangle",
              vertex.size=45,
              vertex.size2=6,
              vertex.label.cex=0.5,
              vertex.color="darkslategray2")
  dev.off()
  return(TRUE)
}
