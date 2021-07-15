#' @title Plot Raven hydrologic process network using DiagrammeR
#'
#' @description
#' This routine takes a connections data from generated using rvn_rvi_connections()
#' and returns the connections information as a DiagrammeR object.
#'
#' @details
#' Uses the output from the \code{\link{rvn_rvi_connections}} function to generate the plot.
#'
#' Note that the output can be also be plotted using the \code{\link{render_graph}} function
#' in the DiagrammeR library.
#'
#' The outputted DiagrammeR object may also have aesthetics modified with various commands from the
#' same library, if desired.
#'
#' @param connections a dataframe of from-to connections generated using rvn_rvi_connections()
#' @param pdfout name of pdf file to save the network plot to, if null no PDF is generated
#'
#' @return \code{d1} returns DiagrammeR object. Also generates a .pdf file in working directory if pdfplot argument is not NULL.
#'
#'
#' @note tries to follow basic network structure, accommodates unrecognized state variables on LHS of plot
#'
#' @seealso \code{\link{rvn_rvi_connections}} to generate connections table from an rvi object
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#' rvi <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR"))
#' conn <- rvn_rvi_connections(rvi)
#'
#' library(DiagrammeR)
#'
#' rvn_rvi_process_diagrammer(conn) %>%
#' render_graph()
#'
#' @importFrom igraph get.data.frame graph_from_data_frame vertex.attributes
#' @importFrom DiagrammeR get_node_df set_node_position create_graph add_node add_edge edge_aes export_graph
#'
#' @export rvn_rvi_process_diagrammer
#'
rvn_rvi_process_diagrammer <- function(connections, pdfout=NULL)
{

  # build vertices from connections data
  verts <- unique( c(connections$From,connections$To) )

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

  #Convert Layout to Dataframe
  layout <- as.data.frame(layout)
  layout$Label <- verts


  # build diagrammer graph and start building, add vertices, add edges, then update positions
  d1 <- create_graph()

  for (i in 1:length(verts)) {
    d1 <- d1 %>% add_node(label=verts[i])
  }

  for (i in 1:nrow(connections)) {

    if (connections$Conditional[i] != "") {
      d1 <- d1 %>% add_edge(from=connections$From[i],
                            to=connections$To[i],
                            edge_aes = edge_aes(
                              color = "red"
                            ),
                            edge_data=list("algorithm"=connections$Algorithm[i],
                                           "processtype"=connections$ProcessType[i],
                                           "conditional"=connections$Conditional[i]))
    } else {
      d1 <- d1 %>% add_edge(from=connections$From[i],
                            to=connections$To[i],
                            edge_data=list("algorithm"=connections$Algorithm[i],
                                           "processtype"=connections$ProcessType[i],
                                           "conditional"=connections$Conditional[i]))
    }
  }


  # set node positions
  nodes_df <- DiagrammeR::get_node_df(d1)

  for (i in 1:nrow(nodes_df)) {

    coords <- layout[layout$Label == nodes_df$label[i], c(1,2)]

    d1 <- d1 %>%
      set_node_position(
        node=i,
        x=coords[,1],y=coords[,2])
  }

  # render graph in viewer
  # render_graph(d1)

  if (!is.null(pdfout)) {export_graph(d1, file_name=pdfout)}
  return(d1)
}
