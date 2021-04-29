## ----Installing RavenR from CRAN, eval=FALSE----------------------------------
#  install.packages("RavenR")

## ----Installing RavenR from Github, eval=FALSE, include=FALSE-----------------
#  # install.packages("devtools")
#  library(devtools)
#  devtools::install_github("rchlumsk/RavenR")

## ----RavenR function list, echo=TRUE, message=FALSE, warning=FALSE, results='hide'----
library(RavenR)

# view first 20 functions in RavenR
ls("package:RavenR") %>% 
  head(., 20) 

## ----Getting help on functions, eval=FALSE------------------------------------
#  ?rvn_flow_scatterplot

## ----R sample data, message=FALSE, warning=FALSE, results='hide'--------------
data("rvn_forcing_data")
# ?rvn_forcing_data
plot(rvn_forcing_data$forcings$temp_daily_ave,
     main="Daily Avg. Temperature")

## ----Raw sample data----------------------------------------------------------
# read in hydrograph sample csv data from RavenR package
ff <- system.file("extdata","run1_Hydrographs.csv", package="RavenR")

# ff is a simple string, which can be substituted with any file location
ff

# read in sample rvi file from the RavenR package
rvi_file <- system.file("extdata", "Nith.rvi", package="RavenR")

# show first 6 lines of the file
readLines(rvi_file) %>% head()

## ----Read forcing data--------------------------------------------------------
ff <- system.file("extdata","run1_ForcingFunctions.csv",package="RavenR")
# ff <- "C:/TEMP/Nith/output/ForcingFunctions.csv" # replace with your own file
ff_data <- RavenR::rvn_forcings_read(ff)
head(ff_data$forcings[,1:6])

## ----Plot forcing data--------------------------------------------------------
myplots <- rvn_forcings_plot(ff_data$forcings)
# myplots$Temperature
# myplots$Radiation
# myplots$AllForcings
myplots$PET

## ----Extract hydrograph data, fig.height=5, fig.width=6-----------------------
ff <- system.file("extdata","run1_Hydrographs.csv",package="RavenR")
# ff <- "mydirectory/Hydrographs.csv" # replace with your own file
hy <- rvn_hyd_read(ff)
head(hy$hyd)
flow36 <- rvn_hyd_extract("Sub36",hy)
precip <- hy$hyd$precip

## ----Plot hydrographs with other utilities, fig.height=5, fig.width=6, message=FALSE, warning=FALSE----
plot(lubridate::date(flow36$sim), flow36$sim,col='red',
     type='l', panel.first=grid())
lines(lubridate::date(flow36$obs), flow36$obs,col='black')

## ----Create hydrograph, fig.height=5, fig.width=6, message=FALSE, warning=FALSE----
rvn_hyd_plot(sim=flow36$sim, obs=flow36$obs, precip=precip)

## ----Spaghetti plot, fig.height=5, fig.width=6--------------------------------
rvn_flow_spaghetti(flow36$sim)

## ----Annual quantiles, fig.height=5, fig.width=6------------------------------
rvn_annual_quantiles(flow36$sim) %>% 
  rvn_annual_quantiles_plot(., ribboncolor='magenta')

## ----Annual peak flows, fig.height=5, fig.width=6-----------------------------
rvn_annual_peak(flow36$sim, obs=flow36$obs) 
rvn_annual_peak_event(flow36$sim, obs=flow36$obs)

## ----Cumulative flow plot and monthly bias, fig.height=5, fig.width=6, message=FALSE, warning=FALSE----
rvn_cum_plot_flow(flow36$sim, obs=flow36$obs) 
rvn_monthly_vbias(flow36$sim, obs=flow36$obs)

## ----Flow dygraphs, fig.height=5, fig.width=6, eval=FALSE, message=FALSE, warning=FALSE----
#  rvn_hyd_dygraph(hy, basins="Sub36")

## ----rvn_apply_wyearly function example, message=FALSE, warning=FALSE---------
myhyd <- system.file("extdata","run1_Hydrographs.csv", package="RavenR") %>% 
  rvn_hyd_read()

library(xts)

# apply mean to calendar year in hydrograph data
xts::apply.yearly(myhyd$hyd$Sub36, mean, na.rm=TRUE)

# apply mean as FUN to daily average temperature
RavenR::rvn_apply_wyearly(myhyd$hyd$Sub36, mean, na.rm=TRUE)

## ----Subbasin map plot, fig.height=5, fig.width=6-----------------------------
# Raw sample data
shpfilename <- system.file("extdata","Nith_shapefile_sample.shp",package="RavenR")

# plot shapefile with baseplot in R
sf::read_sf(shpfilename) -> shp
# plot(shp$geometry)

# Custom Output data from Raven for Nith basin
cust.data <- rvn_custom_read(system.file("extdata","run1_PRECIP_Daily_Average_BySubbasin.csv",
                                     package="RavenR"))

subIDcol <- 'subID' # attribute in shapefile with subbasin IDs
plot.date <- "2003-03-30" # date for which to plot custom data

# function call
rvn_subbasin_map(shpfilename, subIDcol, plot.date, cust.data)

## ----Updated subbasin map plot, fig.height=5, fig.width=6, message=FALSE, warning=FALSE----
library(ggplot2)

# create an updated plot
p1 <- rvn_subbasin_map(shpfilename, subIDcol, plot.date, cust.data)
p1 + ggtitle("Daily Average Precipitation (mm/d)") +
      scale_fill_binned()

## ----Subbasin map of snowpack, fig.height=5, fig.width=6, message=FALSE, warning=FALSE----
cust.data <- rvn_custom_read(system.file("extdata","run1_SNOW_Daily_Average_BySubbasin.csv",
                                     package="RavenR"))
plot.title <- 'Daily Average Snowpack (mm SWE)'
plot.date <- "2003-03-01" # date for which to plot custom data

# create an updated plot, change the colour scheme
rvn_subbasin_map(shpfilename, subIDcol, plot.date, cust.data)+
  ggtitle(plot.title)+
  scale_fill_binned(type="viridis")

## ----rvi connection plot example----------------------------------------------
rvi <- rvn_rvi_read(system.file("extdata","Nith.rvi", package="RavenR"))

rvn_rvi_connections(rvi) %>% 
rvn_rvi_process_plot(., pdfout = NULL)

## ----Read rvh file------------------------------------------------------------
# read in rvh file
rvh <- rvn_rvh_read(system.file("extdata","Nith.rvh", package="RavenR"))

rvh$SBtable[, c("SBID","Downstream_ID","Area","TotalUpstreamArea")]

## ----Discretization network plot example--------------------------------------
# plot network from rvh file directly
plot(rvh$SBnetwork)

# create network plot of watershed structure from rvh file
rvn_subbasin_network_plot(rvh$SBtable, labeled=TRUE)

## ----Write rvt file for flow observation data, message=FALSE, warning=FALSE, eval=FALSE----
#  stations <- c("05CB004","05CA002")
#  
#  # Gather station data/info using tidyhydat functions
#  # library(tidyhydat)
#  # hd <- tidyhydat::hy_daily_flows(station_number = stations,
#  #  start_date = "1996-01-01", end_date = "1997-01-01")
#  
#  # load RavenR package sample data
#  data(rvn_tidyhydat_sample)
#  hd <- rvn_tidyhydat_sample
#  
#  tf1 <- file.path(tempdir(), "station1.rvt")
#  tf2 <- file.path(tempdir(), "station2.rvt")
#  
#  # Create RVT files
#  rvn_rvt_tidyhydat(hd, subIDs=c(3,11),
#    filename=c(tf1,tf2))

## ----Workflow script, eval=FALSE----------------------------------------------
#  modelfolder <- "C:/TEMP/Nith/"  # model folder with Raven.exe and Nith model files
#  fileprefix <- "Nith"            # prefix for model files (i.e. Nith.rvi should be in the modelfolder)
#  outdir <- "./output/"
#  
#  ## if this generates an error for you, the Nith folder cannot be found.
#  # Please update the modelfolder variable accordingly
#  if (!dir.exists(modelfolder)) {stop(sprintf("The folder %s does not exist, please verify!"))}
#  
#  setwd(modelfolder)
#  
#  # RUN RAVEN
#  # =====================================================
#  # writes complete command prompt command
#  # > Raven.exe [filename] -o [outputdir]
#  RavenCMD <- sprintf("Raven.exe %s -o %s", fileprefix, outdir)
#  system(RavenCMD) # this runs raven from the command prompt

## ----Save plots, eval=FALSE---------------------------------------------------
#  # GENERATE OUTPUT PLOTS
#  # =====================================================
#  # read in the model output files
#  
#  ## use ggsave from ggplot2 to save plot pdf
#  ff_data <- rvn_forcings_read(paste0(outdir,"run1_ForcingFunctions.csv"))
#  myplots <- rvn_forcings_plot(ff_data$forcings)
#  myplots$AllForcings %>%
#    ggsave("Forcings.pdf", ., width = 8.5, height=11, units='in')
#  
#  # plot snowpack from xts format, save using base R commands
#  mywshd <- rvn_watershed_read(paste0(outdir, "run1_WatershedStorage.csv"))
#  png("snowpack.png") # create a png file to direct plot to
#  plot(mywshd$watershed_storage$Snow,
#       main='Snowpack (mm SWE)', col='blue')
#  dev.off() #finishes writing plot to .png file

