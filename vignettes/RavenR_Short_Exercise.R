## ---- echo=TRUE, message=FALSE, warning=FALSE, results='hide'------------
library(RavenR)
ls("package:RavenR") # view all functions in RavenR

## ---- message=FALSE, warning=FALSE, results='hide'-----------------------
data(forcing.data)

## ------------------------------------------------------------------------
ff <- forcing.data$forcings
head(ff[,1:6])

## ---- fig.height=6, fig.width=7------------------------------------------
forcings.plot(ff)

## ------------------------------------------------------------------------
data(hydrograph.data)
hy <- hydrograph.data
head(hy$hyd)
flow36 <- hyd.extract("Sub36",hy)
precip <- hyd.extract("precip",hy)$sim

## ---- fig.height=3, fig.width=6, message=FALSE, warning=FALSE------------
plot(lubridate::date(flow36$sim),flow36$sim,col='red',type='l')
lines(lubridate::date(flow36$obs),flow36$obs,col='black')

## ---- fig.height=3, fig.width=6------------------------------------------
hyd.plot(sim=flow36$sim, obs=flow36$obs, precip=precip, range.mult=1.5)

## ---- results='hide'-----------------------------------------------------
ls("package:RavenR")

## ---- eval=FALSE---------------------------------------------------------
#  # Load the RavenR sample data
#  # =====================================================
#  indir <- "C:/temp/Nith/"
#  outdir <- "C:/temp/Nith/output/"
#  fileprefix <- "Nith"
#  
#  if (dir.exists(outdir)==FALSE) {
#    dir.create(outdir)
#  }
#  
#  setwd(outdir)
#  
#  # RUN RAVEN
#  # =====================================================
#  # writes complete command prompt command
#  # > Raven.exe [filename] -o [outputdir]
#  RavenCMD <-paste(indir,"Raven.exe ",indir,fileprefix," -o ",outdir,sep="")
#  system(RavenCMD) # this runs raven from the command prompt

## ---- eval=FALSE---------------------------------------------------------
#  # GENERATE OUTPUT PLOTS
#  # =====================================================
#  # use the package data, or read in the model output files
#  
#  # ff<-forcings.read("ForcingFunctions.csv")
#  pdf("forcings.pdf") # create a pdf file to direct plot to
#  forcings.plot(ff$forcings)
#  dev.off() #finishes writing plot to .pdf file
#  
#  data(watershed.data)
#  mywshd <- watershed.data$watershed.storage
#  #mywshd <- RavenR::watershed.read("WatershedStorage.csv")$watershed.storage
#  png("snowpack.png") # create a png file to direct plot to
#  plot(mywshd$snow)
#  dev.off() #finishes writing plot to .png file

