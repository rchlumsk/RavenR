#' @title Plot Raven hydrologic process network
#'
#' @description
#' This routine takes a connections data from generated using \code{rvn_rvi_connections}
#' and returns the connections information as a network graph ggplot object.
#'
#' @details
#' Uses the output from the \code{\link{rvn_rvi_connections}} function to generate the plot
#' with the \code{ggplot2} library..
#'
#' \code{sv_omit} is used to reduce the clutter in the process plot of state variables that
#' one may wish to omit from the plot.
#'
#' The function uses the functionality from \code{ggrepel} to repel labels from one another.
#' The degree of separation in the labels can be controlled by the \code{repel_force} and
#' \code{lbl_size} parameters (increasing either will increase the separation between labels).
#' The \code{repel_force} may range from approximately 1 to 1e-6. The \code{lbl_size} is a
#' relative estimate of the label height (default 0.5), which is used in estimating the label
#' height in the repel functionality. Providing a larger number will increase the perceived size
#' of the label in the repel functionality and tend towards more separation between labels, and
#' vice-versa. Both of these parameters may need to change depending on the plot size and number
#' of labels.
#'
#' \code{arrow_adj} is the amount that each line segment is reduced in length to accomodate the
#' arrow. Increasing this value will decrease the length of the line segment, and place the arrow
#' further from the box. This value should generally be similar to the \code{arrow_size} parameter.
#'
#' The basic model structure outline is followed, but unrecognized state variables are plotted
#' on the left hand side of the plot (determined with internal RavenR function \code{rvn_rvi_process_layout}).
#'
#' @param rvi_conn a list of connections and AliasTable, provided by \code{rvn_rvi_connections}
#' @param sv_omit character vector of state variables to omit from the plot
#' @param repel_force numeric value indicating the 'force' with which the repel function will move labels
#' @param repel_iter the maximum number of iterations for the repel algorithm
#' @param lbl_size estimated height of labels, used in repel algorithm
#' @param lbl_fill fill colour for labels (default 'lightblue')
#' @param arrow_size size of plotted arrows (default 0.25)
#' @param arrow_adj adjustment in line length reduction for arrows (default 0.25)
#' @param pdfout name of pdf file to save the network plot to, if null no PDF is generated
#'
#' @return {p1}{returns ggplot object. Also generates a .pdf file in working directory if pdfplot argument is not NULL.}
#'
#' @seealso \code{\link{rvn_rvi_connections}} to generate connections table from an rvi object
#' @seealso \code{\link{rvn_rvi_process_diagrammer}} to generate the structure plot using DiagrammeR.
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#'
#' library(ggplot2)
#'
#' p1 <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR")) %>%
#'   rvn_rvi_connections() %>%
#'     rvn_rvi_process_ggplot()
#' p1 ## plot to screen
#'
#' ## change the colour of the background
#' p1 + theme(panel.background = element_rect(fill = 'lightgrey', colour = 'purple'))
#'
#' ## adjust line/arrow colours (no conditional lines shown in Nith example)
#' p1 + scale_colour_manual(values=c('FALSE'='purple', 'TRUE'='red'))
#'
#' ## adjust line/arrow types (no conditional lines shown in Nith example)
#' p1 + scale_linetype_manual(values=c('FALSE'='longdash', 'TRUE'='twodash'))
#'
#' @export rvn_rvi_process_ggplot
#' @importFrom igraph get.data.frame graph_from_data_frame vertex.attributes
#' @importFrom ggplot2 ggplot geom_segment geom_label xlim theme aes arrow unit ggsave scale_colour_manual scale_linetype_manual
rvn_rvi_process_ggplot <- function(rvi_conn,
                                   sv_omit=c("SNOW_DEPTH","COLD_CONTENT","PONDED_WATER/SNOW_LIQ","NEW_SNOW","SNOW_DEFICIT"),
                                   repel_force=1e-3, repel_iter=2000, lbl_size=0.5,
                                   lbl_fill="lightblue", arrow_size=0.25, arrow_adj=0.25,
                                   pdfout=NULL)
{

  x <- y <- NULL

  if (is.null(rvi_conn)) {
    stop("rvn_rvi_process_ggplot: rvi_conn is required")
  } else if (paste(names(rvi_conn),collapse=" ") != "connections AliasTable") {
    stop("rvn_rvi_process_ggplot: rvi_conn must be produced by rvn_rvi_connections, and contain connections and AliasTable.")
  }

  connections <- rvi_conn$connections
  AliasTable <- rvi_conn$AliasTable

  # internal plotting parameters, not intended to change (can be changed by user in plot object)
  # arrow_adj <- 0.25
  # arrow_size <- 0.3
  # lbl_fill <- "lightblue"
  line_color_cond <- "orange"
  line_type_cond <- "dashed"
  line_color_base <- "gray40"
  line_type_base <- "solid"

  from.x <- to.x <- from.y <- to.y <- V1 <- V2 <- Label <- NULL

  # replace all aliased names by their basename for plotting
  if (!is.null(AliasTable)) {
    if (all(c("alias","basename") %in% names(AliasTable))) {
      if (any(AliasTable$alias %in% connections$From)) {
        connections$From[which(connections$From %in% AliasTable$alias)] <-
          AliasTable$basename[match(connections$From[which(connections$From %in% AliasTable$alias)],
                                    table=AliasTable$alias)]
      }
      if (any(AliasTable$alias %in% connections$To)) {
        connections$To[which(connections$To %in% AliasTable$alias)] <-
          AliasTable$basename[match(connections$To[which(connections$To %in% AliasTable$alias)],
                                    table=AliasTable$alias)]
      }

    } else {
      warning("rvn_rvi_process_ggplot: alias and basename should be included in AliasTable. AliasTable will not be used")
      AliasTable <- NULL
    }
  }

  # build vertices from connections data
  verts <- unique( c(connections$From,connections$To) )

  # remove any verts in sv_omit
  if (!is.null(sv_omit)) {
    verts <- verts[verts %notin% sv_omit]

    # update connections with removed sv_omit
    connections <-
      connections[-unique(c(which(connections$From %in% sv_omit), which(connections$To %in% sv_omit))),]
  }

  # build layout for all nodes (to be one by rvn_rvi_process_layout)
  layout <- rvn_rvi_process_layout(verts)

  # nverts<-length(verts)
  # layout<-matrix(1:nverts*2,nrow=nverts,ncol=2)
  # count=1
  #
  # for (i in 1:nverts) {
  #   if      (verts[i]=="ATMOSPHERE"){layout[i,1]=5; layout[i,2]=6;}
  #   else if (verts[i]=="ATMOS_PRECIP"){layout[i,1]=1; layout[i,2]=6.2;}
  #
  #   else if (verts[i]=="CANOPY_SNOW"){layout[i,1]=0; layout[i,2]=5;}
  #   else if (verts[i]=="CANOPY"     ){layout[i,1]=1; layout[i,2]=5.3;}
  #
  #   else if (verts[i]=="SNOW_LIQ"         ){layout[i,1]=-1; layout[i,2]=4;}
  #   else if (verts[i]=="SNOW"         ){layout[i,1]=0; layout[i,2]=4.3;}
  #   else if (verts[i]=="PONDED_WATER" ){layout[i,1]=1; layout[i,2]=4.6;}
  #   else if (verts[i]=="DEPRESSION" ){layout[i,1]=2; layout[i,2]=4.9;}
  #   else if (verts[i]=="WETLAND" ){layout[i,1]=3; layout[i,2]=5.2;}
  #
  #   else if (verts[i]=="SOIL[0]"){layout[i,1]=2; layout[i,2]=3;}
  #   else if (verts[i]=="SURFACE_WATER"    ){layout[i,1]=6; layout[i,2]=3;}
  #
  #   else if (verts[i]=="SOIL[1]"    ){layout[i,1]=2; layout[i,2]=2;}
  #   else if (verts[i]=="FAST_RESERVOIR"    ){layout[i,1]=2; layout[i,2]=2;}
  #
  #   else if (verts[i]=="SOIL[2]"    ){layout[i,1]=2; layout[i,2]=1;}
  #   else if (verts[i]=="SLOW_RESERVOIR"){layout[i,1]=2; layout[i,2]=1;}
  #
  #   else if (verts[i]=="SOIL[3]"    ){layout[i,1]=2; layout[i,2]=0;}
  #
  #   else { layout[i,2]=count; count=count+1;layout[i,1]=-2;}
  # }
  # layout <- as.data.frame(layout)
  # names(layout) <- c("x","y")
  # layout$Label <- verts

  # convert base names in verts back to alias (if provided)
  if (!is.null(AliasTable)) {
    # convert verts back to alias
    if (any(verts %in% AliasTable$basename)) {
      verts[which(verts %in% AliasTable$basename)] <-
        AliasTable$alias[match(verts[which(verts %in% AliasTable$basename)],
                               table=AliasTable$basename)]
    }

    # convert connections back to alias
    if (any(AliasTable$basename %in% connections$From)) {
      connections$From[which(connections$From %in% AliasTable$basename)] <-
        AliasTable$alias[match(connections$From[which(connections$From %in% AliasTable$basename)],
                               table=AliasTable$basename)]
    }
    if (any(AliasTable$basename %in% connections$To)) {
      connections$To[which(connections$To %in% AliasTable$basename)] <-
        AliasTable$alias[match(connections$To[which(connections$To %in% AliasTable$basename)],
                               table=AliasTable$basename)]
    }
    # update layout with alias names
    layout$Label <- verts
  }

  # build network connections for ggplot
  source<-connections$From
  target<-connections$To
  process<-connections$ProcessType
  cond <- unlist(lapply(connections$Conditional, function(x) x!=""))
  nodes<-data.frame(source,target,process,cond)
  network<-graph_from_data_frame(d=nodes,directed=TRUE)

  # shift points using repel_boxes
  bounds <- label_bounds(label=layout$Label,
                         x=layout$x,
                         y=layout$y,
                         height=lbl_size, rotation=0,
                         just="center") %>%
            reformat_bounds()

  xxlim <- list(x=c(min(layout$x)-2, max(layout$x)+2))
  yylim <- list(y=c(min(layout$y)-2, max(layout$y)+2))

  layout[,c("x","y")] <- repel_boxes(data_points=as.matrix(layout[,c("x","y")]),
              boxes=as.matrix(bounds[, c("xmin", "ymin", "xmax", "ymax")]),
              point_padding_x = 0,
              point_padding_y = 0,
              xlim=xxlim$x,
              ylim=yylim$y,
              force=repel_force,
              maxiter=repel_iter,
              direction = "both")
  ## end of repel_boxes section

  #axis limits
  x.min <- min(layout$x)-2
  x.max <- max(layout$x)+2 # previously V2
  # y.min <- min(layout$y)-2
  # y.max <- min(layout$y)+2

  #Convert Network to dataframe (arrow coordinates)
  g <- get.data.frame(network)
  g$from.x <- layout$x[match(g$from,layout$Label)]
  g$from.y <- layout$y[match(g$from,layout$Label)]-0.1
  g$to.x <- layout$x[match(g$to,layout$Label)]
  g$to.y <- layout$y[match(g$to,layout$Label)]+0.1
  g$to.y[g$from.y < g$to.y] <- g$to.y[g$from.y < g$to.y] -arrow_adj #adjust for arrow head

  # add jitter to to.x if is a duplicated line (?)
  # xxx TO DO

  #Create Plot
  p1 <- ggplot()+
    geom_segment(data=g,aes(x=from.x,xend = to.x, y=from.y,yend = to.y, color=cond, linetype=cond),
    arrow = arrow(length = unit(arrow_size, "cm"), type = "closed")) + # , color=line_color
    scale_colour_manual(values=c('FALSE'=line_color_base, 'TRUE'=line_color_cond))+
    scale_linetype_manual(values=c('FALSE'=line_type_base, 'TRUE'=line_type_cond))+
    geom_label(data = layout, aes(x=x, y=y, label = Label), fill = lbl_fill)+
    xlim(c(x.min,x.max))+
    # ylim(c(y.min,y.max))+
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = 'white', colour = 'black'))

  if (!is.null(pdfout)) {ggsave(pdfout,p1, units = "in", height = 7, width = 7, dpi =300)}
  return(p1)
}
