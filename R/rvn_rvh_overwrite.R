#' @describeIn rvn_rvh_write Overwrite contents of original .rvh file
#' @export rvn_rvh_overwrite
#' @importFrom utils write.table
rvn_rvh_overwrite<-function(orig_file,filename,SBtable,HRUtable)
{
  write("# -----------------------------------------------------",append=FALSE,file=filename)
  write("# .rvh file modified by RavenR rvn_rvh_overwrite.R utility ",append=TRUE,file=filename)
  write("# -----------------------------------------------------",append=TRUE,file=filename)

  lineno<-grep(":SubBasins", readLines(orig_file), value = FALSE)
  lineend<-grep(":EndSubBasins", readLines(orig_file), value = FALSE)
  lines=readLines(orig_file)
  a<-1:length(lines)

  noSB<-lines[!  a %in% (lineno+1):(lineend-1) ] # trim out SB table contents

  lineno<-grep(":HRUs", noSB, value = FALSE)
  lineend<-grep(":EndHRUs", noSB, value = FALSE)
  a<-1:length(noSB)

  noHRUs<-noSB[! a %in% (lineno+1):(lineend-1) ] # trim out HRU table contents


  SBstr<-grep(":SubBasins", noHRUs, value = FALSE)
  SBend<-grep(":EndSubBasins", noHRUs, value = FALSE)
  HRUstr<-grep(":HRUs", noHRUs, value = FALSE)
  HRUend<-grep(":EndHRUs", noHRUs, value = FALSE)

  #insert SB contents
  write(noHRUs[1:SBstr],append=TRUE,file=filename)
  write("  :Properties,SBID,Name,Downstream_ID,Profile,ReachLength,Gauged",append=TRUE,file=filename)
  write("  :Units			     ,none,none         ,none		,km				  ,none",append=TRUE,file=filename)
  write.table(SBtable[c('SBID','Name','Downstream_ID','Profile','ReachLength','Gauged')],quote=FALSE,
              row.names=FALSE,col.names=FALSE,sep=", ",append=TRUE,file=filename)

  write(noHRUs[SBend:HRUstr],append=TRUE,file=filename)

  write("  :Attributes,ID, AREA, ELEVATION,LATITUDE,LONGITUDE,BASIN_ID,LAND_USE_CLASS,VEG_CLASS,SOIL_PROFILE,AQUIFER_PROFILE,TERRAIN_CLASS,SLOPE,ASPECT",append=TRUE,file=filename)
  write("  :Units			,km2 , masl     ,deg		 ,deg      ,none		,none          ,none     ,none        ,none           ,none         ,deg  ,degN",append=TRUE,file=filename)

  write.table(HRUtable[c('ID','Area','Elevation','Latitude','Longitude','SBID','LandUse','Vegetation','SoilProfile','Terrain','Aquifer','Slope','Aspect')],quote=FALSE,row.names=FALSE,col.names=FALSE,sep=", ",append=TRUE,file=filename)

  write(noHRUs[HRUend:length(noHRUs)],append=TRUE,file=filename)

  return(TRUE)
}

