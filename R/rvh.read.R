#' Read Raven RVH File
#'
#' rvh.read reads in a given rvh file and generates a data frame of useful
#' summary statistics
#'
#' This function reads in a given Raven rvh file, and calculates a number
#' of useful statistics, including: total upstream area, dominant land cover/
#' vegetation info, and area-weighted aspect/slope/lat/long.
#'
#' Function contributed by Dr. James Craig
#'
#' @param filename file name of the rvh file
#' @return \item{SBtable}{data table of subbasin properties and statistics}
#' @return \item{HRUtable}{data table of HRU properties and statistics}
#' @return \item{SBnetwork}{igraph object for creating a network plot with
#' the plot.subbasinnetwork function}
#'
#' @seealso \code{\link{plot.subbasinNetwork}} for creating a network plot of the watershed
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven rvh subbasin watershed HRU
#' @examples
#'
#' # read in rvh file from Alouette tutorial
#' rvh <- rvh.read('Alouette.rvh')
#'
#' @export rvh.read
rvh.read<-function(filename)
{
  # read subbasins table--------------------------------
  lineno<-grep(":SubBasins", readLines(filename), value = FALSE)
  lineend<-grep(":EndSubBasins", readLines(filename), value = FALSE)
  cnames<-c("SBID","Name","Downstream_ID","Profile","ReachLength","Gauged")
  SubBasinTab<-read.table(filename, skip=lineno+2, nrows=lineend-lineno-3, col.names=cnames,header=FALSE,blank.lines.skip=TRUE, comment.char = "#")

  # read HRUs table ------------------------------------
  lineno<-grep(":HRUs", readLines(filename), value = FALSE)
  lineend<-grep(":EndHRUs", readLines(filename), value = FALSE)
  cnames<-c("ID","Area","Elevation","Latitude","Longitude","SBID","LandUse","Vegetation","SoilProfile","Terrain","Aquifer","Slope","Aspect")
  HRUtab<-read.table(filename, skip=lineno+2, nrows=lineend-lineno-3, col.names=cnames,header=FALSE,blank.lines.skip=TRUE, comment.char = "#")

  # sum area-------------------------------------------
  A<-aggregate(Area ~ SBID, FUN = sum, data=HRUtab)
  out<-A
  Elev<-aggregate(Elevation ~ SBID, FUN = mean, data=HRUtab)
  out<-merge(out, round(Elev,1),by="SBID")

  # area-weighted lat and long--------------------------
  HRUtab$tmpA=HRUtab$Latitude*HRUtab$Area;
  test<-aggregate(tmpA ~ SBID, FUN=sum,data=HRUtab)
  temp<-merge(test,A,by="SBID")
  out$AvgLatit<-temp$tmpA/temp$Area

  HRUtab$tmpA=HRUtab$Longitude*HRUtab$Area;
  test<-aggregate(tmpA ~ SBID, FUN=sum,data=HRUtab)
  temp<-merge(test,A,by="SBID")
  out$AvgLongit<-temp$tmpA/temp$Area

  # mean slope, aspect (area-weighted)
  HRUtab$tmpA=HRUtab$Slope*HRUtab$Area;
  test<-aggregate(tmpA ~ SBID, FUN=sum,data=HRUtab)
  temp<-merge(test,A,by="SBID")
  out$AvgSlope<-temp$tmpA/temp$Area

  HRUtab$tmpA=HRUtab$Aspect*HRUtab$Area;
  test<-aggregate(tmpA ~ SBID, FUN=sum,data=HRUtab)
  temp<-merge(test,A,by="SBID")
  out$AvgAspect<-temp$tmpA/temp$Area

  # delete tmpA
  #HRUtab <- select(HRUtab, -tmpA)

  #dominant land use, vegetation,
  LU<-aggregate(HRUtab$Area,list(HRUtab$LandUse,HRUtab$SBID),FUN=sum)
  colnames(LU)<-c("LandUse","SBID","Area")
  LU<-LU[with(LU, order(SBID, -Area)),]# sort by area cover
  maxlist<-aggregate(LU$Area,list(LU$SBID),FUN=max)
  colnames(maxlist)<-c("SBID","Area")
  newdata<-LU[which(LU$Area %in% maxlist$Area),]
  colnames(newdata)<-c("DomLU","SBID","DomLUArea")
  out<-merge(out,newdata,by="SBID")
  out$DomLUFrac<-round(out$DomLUArea/out$Area*100,2)

  Veg<-aggregate(HRUtab$Area,list(HRUtab$Vegetation,HRUtab$SBID),FUN=sum)
  colnames(Veg)<-c("LandUse","SBID","Area")
  Veg<-Veg[with(Veg, order(SBID, -Area)),]# sort by area cover
  maxlist<-aggregate(Veg$Area,list(Veg$SBID),FUN=max)
  colnames(maxlist)<-c("SBID","Area")
  newdata<-Veg[which(Veg$Area %in% maxlist$Area),]
  colnames(newdata)<-c("DomVeg","SBID","DomVegArea")
  out<-merge(out,newdata,by="SBID")
  out$DomVegFrac<-round(out$DomVegArea/out$Area*100,2)

  out<-merge(SubBasinTab,out,by="SBID")

  #calculate total upstream area
  library('igraph')
  links<-data.frame(SBID=out$SBID,downID=out$Downstream_ID)
  links<-subset.data.frame(links,downID>=0) # get rid of -1

  #create network graph structure
  net <- graph_from_data_frame(d=links, vertices=out, directed=T)
  egon<-ego(net,order=100,nodes=V(net),mode="in")
  size<-ego_size(net,order=100,nodes=V(net),mode="in")
  count=1
  for (i in 1:nrow(out)){
    SBID=out$SBID[i]
    up<-subset.data.frame(out,SBID %in% egon[[i]])
    out$TotalUpstreamArea[i]<-sum(up$Area)
    count=count+1
  }
  #igraph::is_dag mayu be useful here

  return (list(SBtable=out,HRUtable=HRUtab,SBnetwork=net))
}
