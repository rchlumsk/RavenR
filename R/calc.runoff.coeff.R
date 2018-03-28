#' Generate runoff coefficients upstream of gauges
#'
#' Reads model.rvh file and daily avg subbasin precip file (usually PRECIP_Daily_Average_BySubbasin.csv)
#' and generates data frame describing runoff coefficients of gauged basins and observation data coverage.
#' Uses precipitation from entire model run history.
#' Only determines runoff coefficient from available data - prone to overestimation with poor observation coverage.
#'
#' @param rvhfile rvh filename (absolute or relative to working directory, with .rvh extension)
#' @param outdir output directory containing PRECIP_Daily_Average_BySubbasin.csv (absolute or relative to working directory, with ending slash)
#' @param custfile (optional) file name (no directory) for Raven-generated custom output precip-by-subbasin file
#' @param hydfile (optional) file name (no directory) for Raven-generated hydrographs file
#' @param correct (optional) if TRUE, tries to correct runoff coefficient for missing data (assumes missing~0 flow)
#'
#' @return data frame with runoff coefficients of gauged basins
#'
#' @details requires igraph library
#'
#' @author James R. Craig, University of Waterloo
#'
#' @seealso See also the \href{http://raven.uwaterloo.ca/}{Raven web site}
#'
#' @examples
#'
#'   out<-calc.runoff.coeff("example.rvh","C:/TEMP/ouput/")
#'
#' @keywords Raven  rvh  runoff coefficient
#' @export rvh.cleanHRUs

calc.runoff.coeff<-function(rvhfile,outdir,custfile="PRECIP_Daily_Average_BySubbasin.csv",hydfile="Hydrographs.csv",correct=FALSE)
{

custfile<-paste0(outdir,custfile)
hydfile<-paste0(outdir,hydfile)

# open files
#-------------------------------------------------------------
if (!file.exists(custfile)){
  print(paste0("Cannot find ",custfile))
  return (NA)
}
if (!file.exists(rvhfile)){
  print(paste0("Cannot find ",rvhfile))
  return (NA)
}
if (!file.exists(hydfile)){
  print(paste0("Cannot find ",hydfile))
  return (NA)
}
rvh<-rvh.read(rvhfile)
precip<-custom.read(custfile)
hyd<-hyd.read(hydfile)

# get total average precip for length of simulation
#-------------------------------------------------------------
rvh$SBtable$AveAnnPrecip<-colMeans(precip)*365
rvh$SBtable$AveUpstreamPrecip<-rvh$SBtable$AveAnnPrecip*0.0

# generate area-weighted average upstream precip, in mm
#-------------------------------------------------------------
net<-rvh$SBnetwork
egon<-igraph::ego(net,order=100,nodes=igraph::V(net),mode="in")
size<-igraph::ego_size(net,order=100,nodes=igraph::V(net),mode="in")

for (i in 1:nrow(rvh$SBtable)){
  SBID_this=rvh$SBtable$SBID[i]
  up<-subset.data.frame(rvh$SBtable,SBID %in% egon[[i]] | SBID== SBID_this)
  rvh$SBtable$AveUpstreamPrecip[i]<-sum(up$Area*up$AveAnnPrecip)/sum(up$Area)
}

# get list of gauged subbasins, calculate simulate/observed runoff, runoff coeff
#-------------------------------------------------------------
gaugedSBs<-subset.data.frame(rvh$SBtable,rvh$SBtable$Gauged==1)
for (i in 1:nrow(gaugedSBs)){
  #print(gaugedSBs$Name[i])
  thishyd<-hyd.extract(subs=gaugedSBs$Name[i],hyd)
  if (is.null(thishyd$obs)){
    gaugedSBs$obs_cover[i]<-0
    gaugedSBs$simrunoff[i]<-NA
    gaugedSBs$obsrunoff[i]<-NA
  }
  else                     {
    mask<-!is.na(thishyd$obs)
    mask[mask == 0] <- NA
    gaugedSBs$obs_cover[i]<-sum(mask[!is.na(mask)])/nrow(thishyd$obs)
    area<-gaugedSBs$TotalUpstreamArea[i]+gaugedSBs$Area[i]
    gaugedSBs$simrunoff[i]<-mean(thishyd$sim*mask,na.rm = TRUE)*86400*365/(area*1000*1000)*1000
    gaugedSBs$obsrunoff[i]<-mean(thishyd$obs,na.rm = TRUE)*86400*365/(area*1000*1000)*1000
  }
}
gaugedSBs$simrunoff[is.nan(gaugedSBs$simrunoff)]<-NA
gaugedSBs$obsrunoff[is.nan(gaugedSBs$obsrunoff)]<-NA

gaugedSBs$runoff_coeff_sim<-gaugedSBs$simrunoff/gaugedSBs$AveUpstreamPrecip
gaugedSBs$runoff_coeff_obs<-gaugedSBs$obsrunoff/gaugedSBs$AveUpstreamPrecip

if (correct==TRUE){
  gaugedSBs$runoff_coeff_sim<-gaugedSBs$runoff_coeff_sim*gaugedSBs$obs_cover
  gaugedSBs$runoff_coeff_obs<-gaugedSBs$runoff_coeff_obs*gaugedSBs$obs_cover
}
df<-subset(gaugedSBs, select=c(runoff_coeff_sim,runoff_coeff_obs,obs_cover))
row.names(df)<-gaugedSBs$Name

return  (df)
}

# # Example usage:
# runcoefs<-calc.runoff.coeff("reddeer.rvh","output/",correct=TRUE)

# # simple plot:
#  plot(runcoefs$runoff_coeff_sim,col="red",ylim=c(0,1))
#  points(runcoefs$runoff_coeff_obs,col="black")
#
# # bar plot:
# runcoefs<-subset(runcoefs,select=c(runoff_coeff_sim,runoff_coeff_obs))
# par(mar=c(15,5,5,5))
# bp<-barplot(t(as.matrix(runcoefs)), main="Runoff Coefficient Comparison (w/ rough data coverage correction)", ylab = "Runoff coeff", ylim=c(0,1),beside=TRUE, col=c("blue","deepskyblue"),legend.text=c("sim","obs"),las=2)
# ## Add text at top of bars
# text(x = bp, y = c(rbind(runcoefs$runoff_coeff_sim,runcoefs$runoff_coeff_obs)), label = round(c(rbind(obs_cover*100,obs_cover*NA)),0), pos = 3, cex = 0.8, col = "red")



