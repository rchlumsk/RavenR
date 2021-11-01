#' @title Plot subbasin network using visNetwork
#'
#' @description
#' Takes an \code{rvh} object generated using \code{rvn_rvh_read}
#' and returns the connections information of subbasins as an interactive visNetwork graph.
#'
#' @param rvh an \code{rvh} object, provided by \code{rvn_rvh_read}
#' @param groupBy a character referring to one of the sub-basins attributes in the \code{rvh}
#'
#' @return {p1}{returns visNetwork plot object}
#'
#' @seealso \code{\link{rvn_rvh_read}} to import an watershed network table from an \code{rvh} file.
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#'
#' rvh <- rvn_rvh_read(system.file("extdata","Nith.rvh", package="RavenR"))
#' rvn_rvh_subbasin_visnetwork_plot(rvh,groupBy="Gauged")
#' rvn_rvh_subbasin_visnetwork_plot(rvh,groupBy="Elevation")
#'
#' @export rvn_rvh_subbasin_visnetwork_plot
#' @importFrom visNetwork visNetwork visInteraction visOptions visHierarchicalLayout visEdges
#'
rvn_rvh_subbasin_visnetwork_plot<-function(rvh, groupBy="Gauged")
{
  if(missing(rvh)) stop("rvh file missing. See rvn_rvh_read function!")
  if(is.null(rvh$SBtable)) stop("no subcatchments network to show!")
  nodes<-data.frame(id=rvh$SBtable$SBID)
  if(groupBy %in% colnames(rvh$SBtable))
  {
    if(groupBy=="Gauged") rvh$SBtable$Gauged<-ifelse(rvh$SBtable$Gauged=="0","Ungauged","Gauged")
    shapes<-c("square","triangle","circle","diamond","dot","star","ellipse")
    colors<-c("blue","red","orange","grey","purple")
    if(is.numeric(rvh$SBtable[,groupBy]))
    {
       maxClasses<-10
       if(length(unique(rvh$SBtable[,groupBy]))>maxClasses)
       {
          cat("Many groups detected. A classification is performed!\n")
          domain<-range(rvh$SBtable[,groupBy])
          domain<-floor(domain)+c(0,1)
          classes<-seq(domain[1],domain[2],length.out=maxClasses)
          classes<-as.character(cut(rvh$SBtable[,groupBy], breaks = classes))
          rvh$SBtable[,groupBy]<-classes
          classes<-sort(unique(classes))
       }else{
          classes<-sort(unique(rvh$SBtable[,groupBy]))
       }
        colors<-colorRampPalette(c("red","blue"))(length(classes))
        attribMat<-as.data.frame(matrix(NA,nrow(rvh$SBtable),4))
        colnames(attribMat)<-c("color","shape","group","label")
        attribMat$shape<-"square"
        attribMat$group<-paste(groupBy,"=", rvh$SBtable[,groupBy])
        for(i in 1:length(classes))
        {
           attribMat$color[classes[i]==rvh$SBtable[,groupBy]]<-colors[i]
        }
        id<-rvh$SBtable$Downstream_ID==-1
        attribMat$label<-ifelse(id,paste(rvh$SBtable$SBID,"(outlet)"),rvh$SBtable$SBID)
        nodes<-cbind(nodes,attribMat)
    }
    if(!is.numeric(rvh$SBtable[,groupBy]))
    {
       classes<-unique(rvh$SBtable[,groupBy])
       if(length(classes)>length(shapes))
       {
          shapes<-sample(shapes,length(classes),replace = TRUE)
       }else{
          shapes<-shapes[1:length(classes)]
       }
       if(length(classes)>length(colors))
       {
          colors<-sample(colors,length(classes),replace = TRUE)
       }else{
          colors<-colors[1:length(classes)]
       }
       attribMat<-as.data.frame(matrix(NA,nrow(rvh$SBtable),4))
       colnames(attribMat)<-c("color","shape","group","label")
       for(i in 1:length(classes))
       {
          id<-classes[i]==rvh$SBtable[,groupBy]
          attribMat$color[id]<-colors[i]
          attribMat$shape[id]<-shapes[i]
          attribMat$group[id]<-paste(groupBy,"=",classes[i])
       }
       id<-rvh$SBtable$Downstream_ID==-1
       attribMat$label<-ifelse(id,paste(rvh$SBtable$SBID,"(outlet)"),rvh$SBtable$SBID)
       nodes<-cbind(nodes,attribMat)
    }
  }else{
     warning("the selected attribute doesn't exist in the provided rvh file!")
     id<-rvh$SBtable$Downstream_ID==-1
     nodes$shape<-"NA"
     nodes$shape<-ifelse(id,"triangle","square")
     nodes$color<-"NA"
     nodes$color<-ifelse(id,"red","blue")
     nodes$label<-"NA"
     nodes$label<-ifelse(id,paste(rvh$SBtable$SBID,"(outlet)"),rvh$SBtable$SBID)
     nodes$group<-ifelse(nodes$label=="outlet","outlet watershed","watershed")
  }
  edges<-data.frame(from=rvh$SBtable$SBID,to=rvh$SBtable$Downstream_ID)
  edges<-edges[!edges$to==-1,]
  p1<-visNetwork(nodes,edges,width = "100%")            %>%
    visInteraction(navigationButtons = TRUE)            %>%
    visOptions(manipulation = TRUE,selectedBy ="group") %>%
    visHierarchicalLayout(direction = "RL")             %>%
    visEdges(arrows = "to")
  return(p1)
}
