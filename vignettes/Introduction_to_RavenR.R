## ----Installing RavenR from CRAN, eval=FALSE--------------------------------------------------------------------------
#  install.packages("RavenR")

## ----Installing RavenR from Github, eval=FALSE, include=TRUE----------------------------------------------------------
#  # install.packages("devtools")
#  library(devtools)
#  devtools::install_github("rchlumsk/RavenR")

## ----RavenR function list, echo=TRUE, message=FALSE, warning=FALSE, results='hide'------------------------------------
library(RavenR)

# view first 20 functions in RavenR
ls("package:RavenR") %>% 
  head(., 20) 

## ----Getting help on functions, eval=FALSE----------------------------------------------------------------------------
#  ?rvn_flow_scatterplot

## ----R sample data, message=FALSE, warning=FALSE, results='hide'------------------------------------------------------
data("rvn_forcing_data")
# ?rvn_forcing_data
plot(rvn_forcing_data$forcings$temp_daily_ave,
     main="Daily Avg. Temperature")

## ----Raw sample data--------------------------------------------------------------------------------------------------
# read in hydrograph sample csv data from RavenR package
ff <- system.file("extdata","run1_Hydrographs.csv", package = "RavenR")

# ff is a simple string, which can be substituted with any file location
ff

# read in sample rvi file from the RavenR package
rvi_file <- system.file("extdata", "Nith.rvi", package = "RavenR")

# show first 6 lines of the file
readLines(rvi_file) %>% head()

## ----Read forcing data------------------------------------------------------------------------------------------------
ff <- system.file("extdata","run1_ForcingFunctions.csv", package = "RavenR")
# ff <- "C:/TEMP/Nith/output/ForcingFunctions.csv" # replace with your own file
ff_data <- RavenR::rvn_forcings_read(ff)
head(ff_data$forcings[,1:6])

## ----Plot forcing data------------------------------------------------------------------------------------------------
myplots <- rvn_forcings_plot(ff_data$forcings)
# myplots$Temperature
# myplots$Radiation
# myplots$AllForcings
myplots$PET

## ----Plot forcing data with labels------------------------------------------------------------------------------------
library(ggplot2)

myplots <- rvn_forcings_plot(ff_data$forcings)
myplots$Radiation +
  theme(legend.position = "bottom")

## ----Extract hydrograph data, fig.height=5, fig.width=6---------------------------------------------------------------
ff <- system.file("extdata","run1_Hydrographs.csv", package = "RavenR")
# ff <- "mydirectory/Hydrographs.csv" # replace with your own file
hy <- rvn_hyd_read(ff)
head(hy$hyd)
flow36 <- rvn_hyd_extract("Sub36",hy)
precip <- hy$hyd$precip

## ----Plot hydrographs with other utilities, fig.height=5, fig.width=6, message=FALSE, warning=FALSE-------------------
plot(lubridate::date(flow36$sim), flow36$sim,col='red',
     type='l', panel.first=grid())
lines(lubridate::date(flow36$obs), flow36$obs,col='black')

## ----Create hydrograph, fig.height=5, fig.width=6, message=FALSE, warning=FALSE---------------------------------------
rvn_hyd_plot(sim=flow36$sim, obs=flow36$obs, precip = precip)

## ----Spaghetti plot, fig.height=5, fig.width=6------------------------------------------------------------------------
rvn_flow_spaghetti(flow36$sim)

## ----Annual quantiles, fig.height=5, fig.width=6----------------------------------------------------------------------
rvn_annual_quantiles(flow36$sim) %>% 
  rvn_annual_quantiles_plot(., ribboncolor = 'magenta')

## ----Annual peak flows, fig.height=5, fig.width=6---------------------------------------------------------------------
rvn_annual_peak(flow36$sim, obs = flow36$obs) 
rvn_annual_peak_event(flow36$sim, obs = flow36$obs)

## ----Cumulative flow plot and monthly bias, fig.height=5, fig.width=6, message=FALSE, warning=FALSE-------------------
rvn_cum_plot_flow(flow36$sim, obs = flow36$obs) 
rvn_monthly_vbias(flow36$sim, obs = flow36$obs)

## ----Flow dygraphs, fig.height=5, fig.width=6, eval=FALSE, message=FALSE, warning=FALSE-------------------------------
#  library(htmltools)
#  
#  rvn_hyd_dygraph(hy, basins="Sub36") %>%
#  htmltools::tagList()

## ----rvn_apply_wyearly function example, message=FALSE, warning=FALSE-------------------------------------------------
myhyd <- system.file("extdata","run1_Hydrographs.csv", package = "RavenR") %>% 
  rvn_hyd_read()

library(xts)

# apply mean to calendar year in hydrograph data
xts::apply.yearly(myhyd$hyd$Sub36, mean, na.rm = TRUE)

# apply mean as FUN to daily average temperature
RavenR::rvn_apply_wyearly(myhyd$hyd$Sub36, mean, na.rm = TRUE)

## ----RVI connection plot example--------------------------------------------------------------------------------------
rvi <- rvn_rvi_read(system.file("extdata","Nith.rvi", package = "RavenR"))

rvn_rvi_connections(rvi) %>% 
  rvn_rvi_process_ggplot()

## ----RVI write template example---------------------------------------------------------------------------------------
tf <- file.path(tempdir(), "mymodel.rvi")

rvn_rvi_write_template(template_name="HMETS",
                       filename=tf,
                       author="Your Name")

## ----Read rvh file----------------------------------------------------------------------------------------------------
# read in rvh file
rvh <- rvn_rvh_read(system.file("extdata","Nith.rvh", package = "RavenR"))

rvh$SBtable[, c("SBID","Downstream_ID","Area","TotalUpstreamArea")]

## ----Discretization network plot example------------------------------------------------------------------------------
# plot network from rvh file directly
plot(rvh$SBnetwork)

# create network plot of watershed structure from rvh file
rvn_rvh_subbasin_network_plot(rvh$SBtable, labeled = TRUE)

## ----Create RVP template file, eval=FALSE-----------------------------------------------------------------------------
#  rvn_run(fileprefix = "mymodel",
#          rvi_options=":CreateRVPTemplate",
#          showoutput = TRUE)

## ----Fill a basic rvp file, echo=TRUE---------------------------------------------------------------------------------
# temporary file path
tf <- tempfile()

# infill template file with default parameter values
rvn_rvp_fill_template(
                      rvi_file = system.file("extdata","Nith.rvi", package = "RavenR"),
                      rvh_file = system.file("extdata","Nith.rvh", package = "RavenR"),
                      rvp_template_file = system.file("extdata","nithmodel.rvp_temp.rvp", package = "RavenR"),
                      avg_annual_runoff = 123,
                      extra_commands=":RedirectToFile  channel_properties.rvp",
                      rvp_out = tf)

## ----RVP getparams example--------------------------------------------------------------------------------------------
system.file("extdata","Nith.rvi", package = "RavenR") %>%
  rvn_rvi_read() %>% 
  rvn_rvi_getparams() %>% 
  head() # preview of parameter data frame

## ----Write rvt file for flow observation data, message=FALSE, warning=FALSE, eval=FALSE-------------------------------
#  stations <- c("05CB004","05CA002")
#  
#  ## Gather station data/info using tidyhydat functions
#  # library(tidyhydat)
#  # hd <- tidyhydat::hy_daily_flows(station_number = stations,
#  #  start_date = "1996-01-01", end_date = "1997-01-01")
#  
#  ## load RavenR package sample data
#  data(rvn_tidyhydat_sample)
#  hd <- rvn_tidyhydat_sample
#  
#  tf1 <- file.path(tempdir(), "station1.rvt")
#  tf2 <- file.path(tempdir(), "station2.rvt")
#  
#  # Create RVT files
#  rvn_rvt_tidyhydat(hd, subIDs=c(3,11),
#    filename=c(tf1,tf2))
#  
#  # preview first 6 lines of rvt file 1
#  readLines(tf1) %>% head()

## ----write rvt file for meteorological data, message=FALSE, warning=FALSE---------------------------------------------
## Obtain data using the weathercan package
# library(weathercan)
# kam <- weather_dl(station_ids = 51423,
#                   start = "2016-10-01", end = "2019-09-30", interval="day")

## load RavenR package sample data
data(rvn_weathercan_sample)
kam <- rvn_weathercan_sample

fpath1 <- file.path(tempdir(), "met_data.rvt")
fpath2 <- file.path(tempdir(), "met_gauges.rvt")

## basic use, provide temporary file names for writing
## filter for particular columns to write to file
result <- kam[,c("station_name","date","lat","lon","elev","max_temp","min_temp","total_precip")] %>% 
            rvn_rvt_write_met(metdata = ., 
                        filenames = fpath1,
                        filename_stndata = fpath2)

# preview files
readLines(fpath1) %>% head() # data rvt file
readLines(fpath2) %>% head() # gauge data file

## ----Workflow script, eval=FALSE--------------------------------------------------------------------------------------
#  modelfolder <- "C:/TEMP/Nith/"  # model folder with Raven.exe and Nith model files
#  fileprefix <- "Nith"            # prefix for model files (i.e. Nith.rvi should be in the modelfolder)
#  outdir <- "./output/"
#  
#  ## if this generates an error for you, the Nith folder cannot be found.
#  # Please update the modelfolder variable accordingly
#  if (!dir.exists(modelfolder)) {
#    stop(sprintf("The folder %s does not exist, please verify!", modelfolder))
#    }
#  
#  setwd(modelfolder)
#  
#  # RUN RAVEN
#  # =====================================================
#  # writes complete command prompt command (Windows)
#  # > Raven.exe [filename] -o [outputdir]
#  RavenCMD <- sprintf("Raven.exe %s -o %s", fileprefix, outdir)
#  
#  # or adjust string if in a macos/ linux environemnt
#  # RavenCMD <- sprintf("./Raven.exe %s -o %s", fileprefix, outdir)
#  
#  system(RavenCMD) # this runs raven from the command prompt

## ----Save plots, eval=FALSE-------------------------------------------------------------------------------------------
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

## ----Exercise 2 solution, eval=FALSE, include=FALSE-------------------------------------------------------------------
#  library(RavenR)
#  library(tidyhydat)
#  library(weathercan)
#  
#  # search for station name called 'Raven River'
#  stn <- tidyhydat::search_stn_name("Raven River")
#  stn
#  
#  # download daily flow data for this station, and pipe directly to rvn_rvt_tidyhydat
#  tidyhydat::hy_daily_flows(station_number=stn$STATION_NUMBER) %>%
#    rvn_rvt_tidyhydat(., subIDs = 1)
#  
#  # search for meteorological stations within 10km of the WSC gauge station
#  metstn <- weathercan::stations_search(coords=c(stn$LATITUDE,
#                                       stn$LONGITUDE),
#                                     interval="day")
#  metstn
#  
#  # download meteorological data
#  met_data <- weathercan::weather_dl(station_ids = metstn$station_id,
#                         interval="day")
#  
#  # write Raven rvt file
#  rvn_rvt_write_met(met_data)

## ----Exercise 3 solution, eval=FALSE, include=FALSE-------------------------------------------------------------------
#  
#  # view model template options, and keyword needed to use in function
#  ?rvn_rvi_write_template
#  
#  # from documentation,
#  ## The template_name parameter should be one of "UBCWM", "HBV-EC", "HBV-Light",
#  ## "GR4J", "CdnShield", "MOHYSE", "HMETS", "HYPR", or "HYMOD".
#  
#  # write a template rvi file, use the HMETS model configuration again
#  rvn_rvi_write_template(template_name = "HMETS",
#                         filename = "ravenriver.rvi",
#                         author = "Robert Chlumsky",
#                         description = "Template file with HMETS model structure, created for Introduction to RavenR, Exercise 3.")
#  
#  # run Raven to create the template file
#  rvn_run(fileprefix = "ravenriver",
#          rvi_options=":CreateRVPTemplate",
#          showoutput = TRUE)
#  
#  # create a basic rvh file with 1 subbasin 2 hrus
#  #########
#  subdf <- rvn_rvh_blankSBdf(nSubBasins = 1)
#  subdf$Name <- "sub1"
#  
#  hrudf <- rvn_rvh_blankHRUdf(nHRUs = 2, subbasinIDs = c(1))
#  hrudf$Area <- c(100,25)
#  hrudf$Elevation <- 500
#  hrudf$Latitude <-  -114.604170
#  hrudf$Longitude <- 52.072763
#  hrudf$LandUse <- "DEFAULT_LANDUSE"
#  hrudf$Vegetation <- c("FOREST", "URBAN")
#  hrudf$SoilProfile <- "DEFAULT_SOILPROFILE"
#  hrudf$Slope <- 0.005
#  
#  rvn_rvh_write(filename = "ravenriver.rvh",
#                SBtable = subdf,
#                HRUtable = hrudf)
#  
#  # infill template file with default parameter values
#  rvn_rvp_fill_template(fileprefix = "ravenriver",
#                        avg_annual_runoff = 123)

