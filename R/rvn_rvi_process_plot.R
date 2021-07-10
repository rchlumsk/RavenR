#' @title Plot Raven hydrologic process network
#'
#' @description
#' This routine takes a connections data from generated using rvn_rvi_connections()
#' and returns the connections information as a network graph ggplot object.
#'
#' @details
#' This function uses the output from the \code{\link{rvn_rvi_connections}} function to generate the plot.
#'
#' @param connections a dataframe of from-to connections generated using rvn_rvi_connections()
#' @param pdfout name of pdf file to save the network plot to, if null no PDF is generated
#'
#' @return {p1}{returns ggplot object. Also generates a .pdf file in working directory if pdfplot argument is not NULL.}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @note tries to follow basic network structure, accomodates unrecognized state variables on LHS of plot
#'
#' @seealso \code{\link{rvn_rvi_connections}} to generate connections table from an rvi object
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#' rvi <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR"))
#' conn <- rvn_rvi_connections(rvi)
#'
#' rvn_rvi_process_plot(conn)
#'
#' @export rvn_rvi_process_plot
#' @importFrom igraph get.data.frame graph_from_data_frame vertex.attributes
#' @importFrom ggplot2 ggplot geom_segment geom_label xlim theme aes arrow unit ggsave
rvn_rvi_process_plot <- function(connections, pdfout=NULL)
{

  from.x <- to.x <- from.y <- to.y <- V1 <- V2 <- Label <- NULL

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

  #Convert Layout to Dataframe
  layout <- as.data.frame(layout)
  layout$Label <- verts

  #Convert Network to dataframe
  g <- get.data.frame(network)
  g$from.x <- layout$V1[match(g$from,layout$Label)]
  g$from.y <- layout$V2[match(g$from,layout$Label)]-0.1
  g$to.x <- layout$V1[match(g$to,layout$Label)]
  g$to.y <- layout$V2[match(g$to,layout$Label)]+0.1
  g$to.y[g$from.y < g$to.y] <- g$to.y[g$from.y < g$to.y] -0.2 #adjust for arrow head

  #x limits
  x.min <- min(layout$V1)-2
  x.max <- max(layout$V2)+2

  #Create Plot
  p1 <- ggplot()+
    geom_segment(data=g,aes(x=from.x,xend = to.x, y=from.y,yend = to.y),
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "gray40") +
    geom_label(data = layout, aes(x=V1, y=V2, label = Label), fill = "lightblue")+
    xlim(c(x.min,x.max))+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          legend.position = "none")+
    rvn_theme_RavenR()

  if (!is.null(pdfout)) {ggsave(pdfout,p1, units = "in", height = 7, width = 7, dpi =300)}
  return(p1)
}
