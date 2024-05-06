#' @title Plot Raven hydrologic process network using DiagrammeR
#'
#' @description
#' This routine takes a connections data from generated using rvn_rvi_connections()
#' and returns the connections information as a DiagrammeR object.
#'
#' @details
#' Uses the output from the \code{\link{rvn_rvi_connections}} function to generate the plot
#' with the \code{DiagrammeR} library.
#'
#' Note that the output can be plotted using the \code{\link{render_graph}} function
#' in the DiagrammeR library. The outputted DiagrammeR object may also have aesthetics modified
#' with various commands from the
#' same library, if desired, as shown in the examples. The \code{rsvg} and \code{DiagrammeRsvg} packages
#' may be required to export to PDF with desired results,
#' but are not explicit dependencies of RavenR.
#'
#' \code{sv_omit} is used to reduce the clutter in the process plot of state variables that
#' one may wish to omit from the plot.
#'
#' The function uses the functionality from \code{ggrepel} to repel labels from one another.
#' The degree of separation in the labels can be controlled by the \code{repel_force} and
#' \code{lbl_size} parameters (increasing either will increase the separation between labels).
#' The \code{repel_force} may range from approximately 1 to 1e-6. The \code{lbl_size} is a
#' relative estimate of the label height (default 0.5), which is used in estimating the label
#' size in the repel functionality. Providing a larger number will increase the perceived size
#' of the label in the repel functionality and tend towards more separation between labels, and
#' vice-versa. Both of these parameters may need to change depending on the plot size and number
#' of labels. The \code{lbl_height} and \code{lbl_width} parameters can be changed to affect
#' the height and relative width of the actual labels.
#'
#' The basic model structure outline is followed, but unrecognized state variables are plotted
#' on the left hand side of the plot (determined with internal RavenR function \code{rvn_rvi_process_layout}).
#'
#' @param rvi_conn a list of connections and AliasTable, provided by \code{rvn_rvi_connections}
#' @param sv_omit character vector of state variables to omit from the plot
#' @param repel_force numeric value indicating the 'force' with which the repel function will move labels
#' @param repel_iter the maximum number of iterations for the repel algorithm
#' @param lbl_size estimated height of labels, used in repel algorithm
#' @param lbl_height actual height of the labels (in inches)
#' @param lbl_width relative width of the labels (multiplier)
#' @param pdfout name of pdf file to save the network plot to, if null no PDF is generated
#'
#' @return \code{d1} returns DiagrammeR object. Also generates a .pdf file in working directory if
#' pdfplot argument is not \code{NULL}.
#'
#' @seealso \code{\link{rvn_rvi_connections}} to generate connections table from an rvi object
#' @seealso \code{\link{rvn_rvi_process_ggplot}} to generate the structure plot using ggplot.
#'
#' See also the \href{https://raven.uwaterloo.ca/}{Raven page}. Additional details on the
#' \code{DiagrammeR} package may be found on the \href{https://github.com/rich-iannone/DiagrammeR}{Github page}.
#'
#' @examples
#' d1 <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR")) %>%
#'   rvn_rvi_connections() %>%
#'     rvn_rvi_process_diagrammer()
#'
#' # plot diagram using the DiagrammeR package
#' library(DiagrammeR)
#'
#' d1 %>%
#'   render_graph()
#'
#' # modify default plot attributes, plot
#' d1 %>%
#'   select_nodes() %>%
#'   set_node_attrs_ws(node_attr = fillcolor, value = "hotpink") %>%
#'   select_edges() %>%
#'   set_edge_attrs_ws(edge_attr = style, value = "dashed") %>%
#'   set_edge_attrs_ws(edge_attr = penwidth, value = 2) %>%
#'   render_graph()
#'
#' @importFrom igraph get.data.frame graph_from_data_frame vertex.attributes
#' @importFrom DiagrammeR get_node_df set_node_position create_graph add_node add_edge edge_aes node_aes export_graph
#' @export rvn_rvi_process_diagrammer
rvn_rvi_process_diagrammer <- function(rvi_conn,
                                       sv_omit=c("SNOW_DEPTH","COLD_CONTENT","PONDED_WATER/SNOW_LIQ","NEW_SNOW","SNOW_DEFICIT"),
                                       repel_force=1e-3, repel_iter=2000, lbl_size=0.5,
                                       lbl_height=0.3, lbl_width=1.0,
                                       pdfout=NULL)
{

  # nchar_per_inch <- 8 # number of characters per inch in box widths of each state variable

  if (is.null(rvi_conn)) {
    stop("rvn_rvi_process_diagrammer: rvi_conn is required")
  } else if (paste(names(rvi_conn),collapse=" ") != "connections AliasTable") {
    stop("rvn_rvi_process_diagrammer: rvi_conn must be produced by rvn_rvi_connections, and contain connections and AliasTable.")
  }

  connections <- rvi_conn$connections
  AliasTable <- rvi_conn$AliasTable

  # internal plotting parameters, not intended to change (can be changed by user in plot object)
  # arrow_adj <- 0.25
  # arrow_size <- 0.3
  # lbl_fill <- "lightblue"
  lbl_fill="lightblue"
  arrow_size=1.0
  line_color_cond <- "orange"
  line_type_cond <- "dashed"
  line_color_base <- "lightgrey"
  line_type_base <- "solid"
  lbl_fontcolor <- "black"
  lbl_outline_color <- "black"
  lbl_outline_width <- 1.0

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
      warning("rvn_rvi_process_diagrammer: alias and basename should be included in AliasTable. AliasTable will not be used")
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


  # build diagrammer graph and start building, add vertices, add edges, then update positions
  d1 <- create_graph()

  for (i in 1:length(verts)) {
    d1 <- d1 %>% add_node(label=verts[i],
                          node_aes=node_aes(shape='rectangle',
                                            fontname='Helvetica',
                                            fontsize=12,
                                            # width=nchar(verts[i])/nchar_per_inch, # 1 inch width per 8 characters
                                            width=text_grob_length(verts[i])*0.2*lbl_width,
                                            height=lbl_height,
                                            style='filled',
                                            fillcolor=lbl_fill,
                                            fontcolor=lbl_fontcolor,
                                            color=lbl_outline_color,
                                            penwidth=lbl_outline_width))
  }

  for (i in 1:nrow(connections)) {

    if (connections$From[i] %in% verts & connections$To[i] %in% verts) {
      if (connections$Conditional[i] != "") {
        d1 <- d1 %>% add_edge(from=connections$From[i],
                              to=connections$To[i],
                              edge_aes = edge_aes(
                                color = line_color_cond,
                                arrowsize = arrow_size,
                                style = line_type_cond
                              ),
                              edge_data=list("algorithm"=connections$Algorithm[i],
                                             "processtype"=connections$ProcessType[i],
                                             "conditional"=connections$Conditional[i]))
      } else {
        d1 <- d1 %>% add_edge(from=connections$From[i],
                              to=connections$To[i],
                              edge_aes = edge_aes(
                                color = line_color_base,
                                arrowsize = arrow_size,
                                style = line_type_base
                              ),
                              edge_data=list("algorithm"=connections$Algorithm[i],
                                             "processtype"=connections$ProcessType[i],
                                             "conditional"=connections$Conditional[i]))
      }
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
