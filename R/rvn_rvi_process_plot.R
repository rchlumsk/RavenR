#' @title Plot Raven hydrologic network
#'
#' @description
#' This routine takes a connections data from generated using \code{rvn_rvi_connections}
#' and returns the connections information as a network graph.
#'
#' @param rvi_conn a list of connections and AliasTable, provided by \code{rvn_rvi_connections}
#'
#' @return {p1}{returns GGally plot}
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
#'   rvn_rvi_process_plot()
#'
#'
#' @export rvn_rvi_process_plot
#' @importFrom network network set.vertex.attribute
#' @importFrom ggnet2 GGally

rvn_rvi_process_plot<-function(connections)
{
   storages<-unique(c(unique(connections$From),unique(connections$To)))
   con<-matrix(0,length(storages),length(storages))
   colnames(con)<-rownames(con)<-storages
   for(i in 1:length(storages))
   {
      for(j in 1:length(storages))
      {
         for(k in 1:nrow(connections))
         {
            if(rownames(con)[i]==connections$From[k] &
               colnames(con)[j]==connections$To[k])
            {
               con[i,j]<-1
            }
         }
      }
   }
   types<-storages
   color.palette<-1:length(types)
   shape.palette<-10:(10+length(types)-1)
   size.palette<-rep(5,length(types))
   names(size.palette)<-types
   names(shape.palette)<-types
   names(color.palette)<-types
   net<-network(con)
   set.vertex.attribute(net,"type",types)
   ggnet2(net,color='type',size='type',
          shape='type',
          color.palette=color.palette,
          shape.palette=shape.palette,
          size.palette=size.palette,
          arrow.size = 5, arrow.gap = 0.025) +
          guides(size = FALSE)
}
