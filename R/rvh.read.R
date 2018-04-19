#' Read Raven .rvh (watershed discretization) file
#'
#' This routine reads in a valid Raven watershed discretization (.rvh) file and returns the
#' information about HRUs and Subbasins as data tables. It also returns a subbasin igraph
#' network object which describes stream network connectivity and adds additional HRU-derived
#' subbasin characteristics such as total upstream area and dominant land/vegetation classes.
#'
#' @param filename the name of the .rvh file (with .rvh extension included ), either relative to the working directory or absolute.
#'
#' @return
#' Returns a list including:
#' \item{SBtable}{a data table of Subbasin characteristics indexed by Subbasin ID (SBID). Includes
#' the following data columns from the .rvh file : SBID, Name, Downstream_ID, Profile, ReachLength,
#' Gauged. The rvh.read() functions supplements this with additional columns: Area, Elevation, AvgLatit,
#' AvgLongit, AvgSlope, AvgAspect, DomLU, DomLUArea, DomLUFrac, DomVeg, DomVegArea, DomVegFrac.
#' Elevation, AvgLatit, AvgLongit, AvgSlope, and AvgAspect are the area-weighted averages from all
#' constituent HRUs. DomLU is the dominant land use name, DomLUArea is the area (in km2) of the
#' dominant land use and DomLUArea is the percentage of the basin covered with DomLU; same applies to DomVeg.}
#'
#' \item{HRUtable}{a data table of HRU characteristics, with land use and vegetation classes as factors.
#' Contains identical information as found in the :HRUs-:EndHRUs block of the .rvh file: ID, Area,
#' Elevation, Latitude, Longitude, SBID, LandUse,Vegetation, SoilProfile, Terrain, Aquifer, Slope,
#' and Aspect.}
#'
#' \item{SBnetwork}{an igraph network graph network describing subbasin stream network connectivity,
#' with nodes indexed by SBID.}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @note depends upon igraph library; does not like tabs in the .rvh file - it should be untabified first.
#' The .rvh file can have arbitrary contents outside of the :HRUs-:EndHRUs and :SubBasins-:EndSubBasins
#' command blocks.
#'
#' @details does not like comma-delimited tables with a trailing comma
#'
#' @seealso
#' \code{\link{rvh.write}} to write contents of the generated (and usually modified HRU and SubBasin tables)
#' \code{\link{subbasinNetwork.plot}} to plot the subbasin network
#' See also the \href{http://raven.uwaterloo.ca/}{Raven page}
#'
#' @examples
#'  \dontrun{
#'   # sample workflow of rvh.read
#'
#'   rvh<-rvh.read("example.rvh")
#'
#'   # get number of HRUs
#'   numHRUs<-nrow(rvh$HRUtable)
#'
#'   # total watershed area
#'   watershed.area<-sum(rvh$HRUtable$Area)
#'
#'   # sub-table of headwater subbasins
#'   headwaterBasins<-subset(rvh$SBtable,TotalUpstreamArea==0)
#'
#'   # sub-table of Forested HRUs
#'   forestHRUs<-subset(rvh$HRUtable,LandUse=="FOREST")
#'
#'   # get total area upstream of subbasin "Raven_River" outlet
#'   upstr<-(rvh$SBtable$TotalUpstreamArea+rvh$SBtable$Area)
#'   gauge_area<-upstr[rvh$SBtable$Name=="Raven_River"]
#'  }
#' @keywords Raven  rvh  HRUs  SubBasins
#' @export rvh.read
rvh.read<-function(filename)
{
  stopifnot(file.exists(filename))


  # read subbasins table--------------------------------
  lineno<-grep(":SubBasins", readLines(filename), value = FALSE)
  lineend<-grep(":EndSubBasins", readLines(filename), value = FALSE)

  if ((length(lineno)==0) || (length(lineend)==0)){
    print('warning: filename not a valid .rvh file (no :SubBasins block)')
  }
  delim=""
  if (length(grep(",", readLines(filename)[(lineno+3):(lineend-1)], value = FALSE))>0){
    delim=","
  }
  cnames<-c("SBID","Name","Downstream_ID","Profile","ReachLength","Gauged")
  #print(paste0("read sbs: |",delim,"| ",lineno," ",lineend," ",lineend-lineno-3 ))
  SubBasinTab<-read.table(filename, skip=lineno+2, nrows=lineend-lineno-3, sep=delim,col.names=cnames,header=FALSE,blank.lines.skip=TRUE, strip.white=TRUE,stringsAsFactors=FALSE,flush=TRUE,comment.char = "#")
  SubBasinTab$Name<-trimws(SubBasinTab$Name)
  #print('done reading sbs')
  #untabify
  #SubBasinTab <- as.data.frame(sapply(SubBasinTab, function(x) gsub("\t", "", x)))


  # read HRUs table ------------------------------------
  lineno<-grep(":HRUs", readLines(filename), value = FALSE)
  lineend<-grep(":EndHRUs", readLines(filename), value = FALSE)
  if ((length(lineno)==0) || (length(lineend)==0)){
    print('warning: filename not a valid .rvh file (no :HRUs block)')
  }
  delim=""
  if (length(grep(",", readLines(filename)[(lineno+3):(lineend-1)], value = FALSE))>0){
    delim=","
  }
  cnames<-c("ID","Area","Elevation","Latitude","Longitude","SBID","LandUse","Vegetation","SoilProfile","Terrain","Aquifer","Slope","Aspect")

  #print(paste0("read HRUs: |",delim,"| ",lineno," ",lineend," ",lineend-lineno-3 ))
  HRUtab<-read.table(filename, skip=lineno+2, nrows=lineend-lineno-3, sep=delim,col.names=cnames,header=FALSE,blank.lines.skip=TRUE,strip.white=TRUE,stringsAsFactors=FALSE,flush=TRUE,comment.char = "#")
  #print('done reading HRUs')
  #untabify
  #HRUtab <- as.data.frame(sapply(HRUtab, function(x) gsub("\t", "", x)))


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
  HRUtab<-HRUtab[ , !(names(HRUtab) %in% c("tmpA"))]

  #dominant land use, vegetation,
  LU<-aggregate(HRUtab$Area,list(HRUtab$LandUse,HRUtab$SBID),FUN=sum)
  colnames(LU)<-c("LandUse","SBID","Area")
  LU<-LU[with(LU, order(SBID, -Area)),]# sort by area cover
  maxlist<-aggregate(LU$Area,list(LU$SBID),FUN=max)
  colnames(maxlist)<-c("SBID","Area")
  newdata<-LU[which(LU$Area %in% maxlist$Area),]
  colnames(newdata)<-c("DomLU","SBID","DomLUArea")
  newdata <- newdata[ !duplicated(newdata$SBID), ]
  out<-merge(out,newdata,by="SBID") # something wrong with this merge
  out$DomLUFrac<-round(out$DomLUArea/out$Area*100,2)

  Veg<-aggregate(HRUtab$Area,list(HRUtab$Vegetation,HRUtab$SBID),FUN=sum)
  colnames(Veg)<-c("LandUse","SBID","Area")
  Veg<-Veg[with(Veg, order(SBID, -Area)),]# sort by area cover
  maxlist<-aggregate(Veg$Area,list(Veg$SBID),FUN=max)
  colnames(maxlist)<-c("SBID","Area")
  newdata<-Veg[which(Veg$Area %in% maxlist$Area),]
  colnames(newdata)<-c("DomVeg","SBID","DomVegArea")
  newdata <- newdata[ !duplicated(newdata$SBID), ]
  out<-merge(out,newdata,by="SBID")
  out$DomVegFrac<-round(out$DomVegArea/out$Area*100,2)

  out<-merge(SubBasinTab,out,by="SBID")

  row.names(out)<-out$SBID

  #calculate total upstream area
  #library('igraph')
  links<-data.frame(SBID=out$SBID,downID=out$Downstream_ID)
  links<-subset.data.frame(links,downID>=0) # get rid of -1

  #create network graph structure
  net <-igraph::graph_from_data_frame(d=links, vertices=out, directed=T)
  egon<-igraph::ego(net,order=100,nodes=igraph::V(net),mode="in")
  size<-igraph::ego_size(net,order=100,nodes=igraph::V(net),mode="in")
  count=1
  for (i in 1:nrow(out)){
    SBID=out$SBID[i]
    up<-subset.data.frame(out,SBID %in% egon[[i]])
    out$TotalUpstreamArea[i]<-sum(up$Area)
    count=count+1
  }

  return (list(SBtable=out,HRUtable=HRUtab,SBnetwork=net))
}
