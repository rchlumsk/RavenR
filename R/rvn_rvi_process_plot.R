#' @title Plot Raven hydrologic network
#'
#' @description
#' This routine takes a connections data from generated using \code{rvn_rvi_connections}
#' and returns the connections information as a network graph.
#'
#' @param rvi_conn a list of connections and AliasTable, provided by \code{rvn_rvi_connections}
#' @param custom_label (optional) a two-columns matrix/data.frame in which the first and the second columns are  equal the hydrologic compartment labels in the \code{rvi_conn} and their corresponding replacement labels respectively provided that \code{default_label = FALSE}
#' @param default_label (optional) logical. if \code{TRUE} an internal default labels are used as the compartments names given that \code{custom_label = NULL}
#'
#' @return {p1}{returns visNetwork plot}
#'
#' @seealso \code{\link{rvn_rvi_connections}} to generate connections table from an rvi object
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#'
#' library(GGally)
#'
#'   rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR")) %>%
#'   rvn_rvi_connections() %>%
#'   rvn_rvi_process_plot(.,default_label=TRUE)
#'
#' @export rvn_rvi_process_plot
#' @importFrom visNetwork visNetwork visInteraction visOptions

rvn_rvi_process_plot<-function(rvi_conn,custom_label=NULL,default_label=FALSE)
{
   if(missing(rvi_conn)) stop("connection matrix required! Use rvn_rvi_connections function.")
   AliasTable<-rvi_conn$AliasTable
   rvi_conn<-rvi_conn$connections

   # handling a missing connection
   if(any(rvi_conn$From=="SNOW_TEMP"))
   {
     if(rvi_conn$To[rvi_conn$From=="SNOW_TEMP"]=="")
     {
       rvi_conn$To[rvi_conn$From=="SNOW_TEMP"]<-"ATMOSPHERE"
     }
   }

   # creating a connection matrix
   storages<-unique(c(unique(rvi_conn$From),unique(rvi_conn$To)))
   con<-matrix(0,length(storages),length(storages))
   colnames(con)<-rownames(con)<-storages
   for(i in 1:length(storages))
   {
     for(j in 1:length(storages))
     {
       for(k in 1:nrow(rvi_conn))
       {
         if(rownames(con)[i]==rvi_conn$From[k] &
            colnames(con)[j]==rvi_conn$To[k])
         {
           con[i,j]<-1
         }
       }
     }
   }

   # assigning custom labels/labels handling
   c.names<-colnames(con)
   if(!is.null(AliasTable))
   {
      for(i in 1:nrow(AliasTable))
      {
         c.names[c.names==AliasTable$alias[i]]<-AliasTable$basename[i]
      }
   }

   if(!is.null(custom_label) & !default_label)
   {
      if(ncol(custom_label)!=2) stop ("custom_label must be a two collumn matrix/data.frame.")
      idReplacements<-match(c.names,custom_label[,1])
      c.names<-custom_label[idReplacements[!is.na(idReplacements)],2]
   }
   if(missing(custom_label) & default_label)
   {
      ProcConDataFile<-system.file("extdata","processesLabels.dat", package="RavenR")
      stopifnot(file.exists(ProcConDataFile))
      load(ProcConDataFile)
      idReplacements<-match(c.names,labels$name)
      c.names<-labels$replacement[idReplacements[!is.na(idReplacements)]]
   }
   colnames(con)<-rownames(con)<-c.names

   # network nodes and edges creation
   nodes<-data.frame(id=1:nrow(con),label=colnames(con),shape="square")
   relations<-apply(con==1,1,which)
   edges<-data.frame(row.names=c("from","to"))
   for(i in 1:length(relations))
   {
      if(length(relations[[i]])>0)
      {
        currentEdges<-cbind(i,relations[[i]])
        colnames(currentEdges)<-c("from","to")
        edges<-rbind(edges,currentEdges)
      }
   }
   rownames(edges)<-1:nrow(edges)

   p1<-visNetwork(nodes,edges)                 %>%
       visInteraction(navigationButtons = TRUE)%>%
       visOptions(manipulation = TRUE)
   return(p1)
}
