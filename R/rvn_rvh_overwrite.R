#' @describeIn rvn_rvh_write Overwrite contents of original .rvh file
#' @export rvn_rvh_overwrite
#' @importFrom utils write.table
rvn_rvh_overwrite<-function(filename,SBtable,HRUtable,basefile)
{

  # lineno<-grep(":SubBasins", readLines(basefile), value = FALSE)
  # lineend<-grep(":EndSubBasins", readLines(basefile), value = FALSE)
  lines=readLines(basefile)
  # a<-1:length(lines)

  SBstr<-grep(":SubBasins", lines, value = FALSE)
  SBend<-grep(":EndSubBasins", lines, value = FALSE)
  HRUstr<-grep(":HRUs", lines, value = FALSE)
  HRUend<-grep(":EndHRUs", lines, value = FALSE)

  write("# -----------------------------------------------------",append=FALSE,file=filename)
  write("# .rvh file modified by RavenR rvn_rvh_overwrite.R utility ",append=TRUE,file=filename)
  write("# -----------------------------------------------------",append=TRUE,file=filename)

  # noSB<-lines[!  a %in% (lineno+1):(lineend-1) ] # trim out SB table contents
  #
  # lineno<-grep(":HRUs", noSB, value = FALSE)
  # lineend<-grep(":EndHRUs", noSB, value = FALSE)
  # a<-1:length(noSB)
  #
  # noHRUs<-noSB[! a %in% (lineno+1):(lineend-1) ] # trim out HRU table contents

  # write upper rvh file lines
  write(lines[1:(SBstr-1)],append=TRUE,file=filename)
  # write("\n",append=TRUE,file=filename)

  #insert SB contents
  # write("  :Properties,SBID,Name,Downstream_ID,Profile,ReachLength,Gauged",append=TRUE,file=filename)
  # write("  :Units			     ,none,none         ,none		,km				  ,none",append=TRUE,file=filename)
  # write.table(SBtable[c('SBID','Name','Downstream_ID','Profile','ReachLength','Gauged')],quote=FALSE,
  #             row.names=FALSE,col.names=FALSE,sep=", ",append=TRUE,file=filename)

  #-- SubBasins
  rvn_write_Raven_label('SubBasins',filename)
  rvn_write_Raven_table(attributes = c('Attributes','NAME','DOWNSTREAM_ID','PROFILE','REACH_LENGTH','GAUGED'),
                        units = c('Units','none','none','none','km','none'),
                        df = SBtable[c('SBID','Name','Downstream_ID','Profile','ReachLength','Gauged')],
                        filename = filename)
  rvn_write_Raven_label('EndSubBasins\n',filename)


  write(lines[(SBend+1):(HRUstr-1)],append=TRUE,file=filename)
  # write("\n",append=TRUE,file=filename)

  # write("  :Attributes,ID, AREA, ELEVATION,LATITUDE,LONGITUDE,BASIN_ID,LAND_USE_CLASS,VEG_CLASS,SOIL_PROFILE,AQUIFER_PROFILE,TERRAIN_CLASS,SLOPE,ASPECT",append=TRUE,file=filename)
  # write("  :Units			,km2 , masl     ,deg		 ,deg      ,none		,none          ,none     ,none        ,none           ,none         ,deg  ,degN",append=TRUE,file=filename)
  # write.table(HRUtable[c('ID','Area','Elevation','Latitude','Longitude','SBID','LandUse','Vegetation','SoilProfile','Terrain','Aquifer','Slope','Aspect')],quote=FALSE,row.names=FALSE,col.names=FALSE,sep=", ",append=TRUE,file=filename)

  #-- HRUs
  rvn_write_Raven_label("HRUs",filename)
  rvn_write_Raven_table(attributes = c('Attributes','AREA', 'ELEVATION','LATITUDE','LONGITUDE','BASIN_ID','LAND_USE_CLASS',
                                       'VEG_CLASS','SOIL_PROFILE','AQUIFER_PROFILE','TERRAIN_CLASS','SLOPE','ASPECT'),
                        units = c('Units','km2','masl','deg','deg','none','none','none','none','none','none','deg','degN'),
                        df = HRUtable[c('ID','Area','Elevation','Latitude','Longitude','SBID','LandUse',
                                        'Vegetation','SoilProfile','Aquifer','Terrain','Slope','Aspect')],
                        filename = filename)
  rvn_write_Raven_label("EndHRUs\n", filename)


  # write remaining rvh file lines
  write(lines[(HRUend+1):length(lines)],append=TRUE,file=filename)
  write("\n",append=TRUE,file=filename)

  return(TRUE)
}

