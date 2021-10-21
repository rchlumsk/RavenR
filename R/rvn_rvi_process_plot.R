#' @title Plot Raven hydrologic network
#'
#' @description
#' This routine takes a connections data from generated using \code{rvn_rvi_connections}
#' and returns the connections information as a network graph.
#'
#' @param rvi_conn a list of connections and AliasTable, provided by \code{rvn_rvi_connections}
#' @param custom_label a two columns matrix/data.frame in which the first and the second columns are  equal the hydrologic compartment labels in the \code{rvi_conn} and their corresponding replacement labels respectively
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

rvn_rvi_process_plot<-function(rvi_conn,custom_label)
{
   if(missing(rvi_conn)) stop("connection matrix required! Use rvn_rvi_connections function.")

   # assigning custom labels
   if(!missing(custom_label))
   {
     if(ncol(custom_label)!=2) stop ("custom_label must be a two collumn matrix/data.frame.")
     for(i in 1:nrow(custom_label))
     {
       idFrom<-which(!is.na(match(rvi_conn$From,custom_label[i,1])))
       idTo <-which(!is.na(match(rvi_conn$To,   custom_label[i,1])))
       if(length(idFrom)>0) rvi_conn$From[idFrom]<-custom_label[i,2]
       if(length(idTo)>0)   rvi_conn$To[idTo]    <-custom_label[i,2]
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
   types<-storages
   color.palette<-1:length(types)
   shape.palette<-10:(10+length(types)-1)
   size.palette<-rep(5,length(types))
   names(size.palette)<-types
   names(shape.palette)<-types
   names(color.palette)<-types

   # making a network using the connection matrix
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
