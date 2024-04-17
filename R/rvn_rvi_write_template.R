#' @title Write Raven rvi file based on model configuration templates
#'
#' @description
#' Writes a Raven rvi file based on one of several template model configurations.
#'
#' @param template_name name of the model template to be written (default 'UBCWM')
#' @param filename Name of the rvi file, with extension (optional)
#' @param overwrite boolean whether to overwrite file if it already exists (default \code{FALSE})
#' @param writeheader boolean whether to write a header to the rvi file (default \code{TRUE})
#' @param filetype File extension, Encoding, Raven version (e.g. "rvp ASCII Raven 2.9.1") (optional)
#' @param author Name of file author (optional)
#' @param description File Description for header (e.g., Basin or project information, R script name) (optional)
#'
#' @return \item{TRUE}{returns \code{TRUE} if executed successfully}
#'
#' @details
#' Raven has the capability of emulating a number of existing model configurations, and a number of additional
#' novel model configurations are provided which may be helpful to the user. These can be written with this function
#' for ease of getting started with a model using Raven.
#'
#' The template_name parameter should be one of "UBCWM", "HBV-EC", "HBV-Light", "GR4J",
#'  "CdnShield", "MOHYSE", "HMETS", "HYPR", "HYMOD", or "SAC-SMA".
#'
#' This function uses the same model template files that are provided in the Raven User's manual, Appendix D.
#'
#' The \code{\link{rvn_write_Raven_newfile}} is used to write a header in the rvi file. Writing of a header
#' can be disabled with \code{writeheader=FALSE}.
#'
#' @examples
#'
#' # write the Canadian Shield configuration to 'mymodel.rvi'
#' rvn_rvi_write_template(template_name="CdnShield",
#'    filename=file.path(tempdir(), "mymodel.rvi"))
#'
#' # write the HMETS model with some additional details in the description
#' rvn_rvi_write_template(template_name="HMETS",
#'    filename=file.path(tempdir(), "mynewmodel.rvi"),
#'    author="Robert Chlumsky",
#'    description="RVI file for the HMETS model (Martel, 2017) created by RavenR")
#'
#' @export rvn_rvi_write_template
rvn_rvi_write_template <- function(template_name="UBCWM", filename=NULL,
                                   overwrite=TRUE, writeheader=TRUE,
                                   filetype="rvi ASCII Raven", author="RavenR",
                                   description=NULL) {

  known_templates  <- c("UBCWM", "HBV-EC", "HBV-Light", "GR4J", "CdnShield", "MOHYSE", "HMETS", "HYPR", "HYMOD", "SAC-SMA")

  if (is.null(template_name) | template_name %notin% known_templates) {
    stop("template_name must be one of the available model templates, see function details")
  }

  if (is.null(filename)) {
    filename  <- sprintf("%s_template.rvi", template_name)
  } else if (rvn_substrRight(filename,4) != ".rvi") {
    warning("filename should end in .rvi, extension will be added to filename")
    filename <- sprintf("%s.rvi", filename)
  }

  if (!overwrite & file.exists(filename)) {
    stop(sprintf("Filename %s already exists and overwrite set as FALSE",filename))
  }

  if (writeheader) {

    if (is.null(description)) {
      description <- sprintf("File template for %s model, written using RavenR::rvn_rvi_write_template", template_name)
    }

    rvn_write_Raven_newfile(filename=filename, description=description, filetype=filetype, author=author)
    # fileheader <- readLines(filename)
  }

  model_templates <- list(

    "UBCWM"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0
#
:Method              ORDERED_SERIES
:Interpolation       INTERP_NEAREST_NEIGHBOR

:Routing             ROUTE_NONE
:CatchmentRoute      ROUTE_DUMP

:Evaporation         PET_MONTHLY_FACTOR
:OW_Evaporation      PET_MONTHLY_FACTOR
:SWRadiationMethod   SW_RAD_UBCWM
:SWCloudCorrect      SW_CLOUD_CORR_UBCWM
:SWCanopyCorrect     SW_CANOPY_CORR_UBCWM
:LWRadiationMethod   LW_RAD_UBCWM
:WindspeedMethod     WINDVEL_UBCWM
:RainSnowFraction    RAINSNOW_UBCWM
:PotentialMeltMethod POTMELT_UBCWM
:OroTempCorrect      OROCORR_UBCWM
:OroPrecipCorrect    OROCORR_UBCWM2
:OroPETCorrect       OROCORR_UBCWM
:CloudCoverMethod    CLOUDCOV_UBCWM
:PrecipIceptFract    PRECIP_ICEPT_USER
:MonthlyInterpolationMethod  MONTHINT_LINEAR_21

:SoilModel          SOIL_MULTILAYER 6
:SnapshotHydrograph

# --Hydrologic Processes-------------------------
:Alias TOP_SOIL      SOIL[0]
:Alias INT_SOIL      SOIL[1]
:Alias SHALLOW_GW    SOIL[2]
:Alias DEEP_GW       SOIL[3]
:Alias INT_SOIL2     SOIL[4]
:Alias INT_SOIL3     SOIL[5]
:HydrologicProcesses
   :SnowAlbedoEvolve  SNOALB_UBCWM
   :SnowBalance       SNOBAL_UBCWM     MULTIPLE      MULTIPLE
   # moves snowmelt to fast runoff
   :Flush             RAVEN_DEFAULT    PONDED_WATER  INT_SOIL2
     :-->Conditional    HRU_TYPE IS GLACIER
   :GlacierMelt	      GMELT_UBC        GLACIER_ICE   PONDED_WATER
   :Precipitation     PRECIP_RAVEN     ATMOS_PRECIP  MULTIPLE
   :SoilEvaporation   SOILEVAP_UBC     MULTIPLE      ATMOSPHERE
   :Infiltration      INF_UBC          PONDED_WATER  MULTIPLE
   # from infiltration to routing
   :Flush             RAVEN_DEFAULT    SURFACE_WATER INT_SOIL2
   :GlacierInfiltration	GINFIL_UBCWM   PONDED_WATER  MULTIPLE
   # soils really used as routing stores
   :Percolation       PERC_LINEAR_ANALYTIC INT_SOIL  INT_SOIL2
   :Percolation       PERC_LINEAR_ANALYTIC INT_SOIL2 INT_SOIL3
   :Baseflow          BASE_LINEAR      INT_SOIL3     SURFACE_WATER
   :Baseflow          BASE_LINEAR      SHALLOW_GW    SURFACE_WATER
   :Baseflow          BASE_LINEAR      DEEP_GW       SURFACE_WATER
   :GlacierRelease    GRELEASE_LINEAR  GLACIER       SURFACE_WATER
:EndHydrologicProcesses

#

",
    "HBV-EC"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0
#
# --Model Details -------------------------------
:Method              ORDERED_SERIES
:Interpolation       INTERP_NEAREST_NEIGHBOR

:Routing             ROUTE_NONE
:CatchmentRoute      ROUTE_TRI_CONVOLUTION

:Evaporation         PET_FROMMONTHLY
:OW_Evaporation      PET_FROMMONTHLY
:SWRadiationMethod   SW_RAD_DEFAULT
:SWCloudCorrect      SW_CLOUD_CORR_NONE
:SWCanopyCorrect     SW_CANOPY_CORR_NONE
:LWRadiationMethod   LW_RAD_DEFAULT
:RainSnowFraction    RAINSNOW_HBV
:PotentialMeltMethod POTMELT_HBV
:OroTempCorrect      OROCORR_HBV
:OroPrecipCorrect    OROCORR_HBV
:OroPETCorrect       OROCORR_HBV
:CloudCoverMethod    CLOUDCOV_NONE
:PrecipIceptFract    PRECIP_ICEPT_USER
:MonthlyInterpolationMethod  MONTHINT_LINEAR_21

:SoilModel           SOIL_MULTILAYER 3

# --Hydrologic Processes-------------------------
:Alias       FAST_RESERVOIR SOIL[1]
:Alias       SLOW_RESERVOIR SOIL[2]

# an oddity unique to HBV:
:LakeStorage SLOW_RESERVOIR

:HydrologicProcesses
  :SnowRefreeze      FREEZE_DEGREE_DAY  SNOW_LIQ        SNOW
  :Precipitation     PRECIP_RAVEN       ATMOS_PRECIP    MULTIPLE
  :CanopyEvaporation CANEVP_ALL         CANOPY          ATMOSPHERE
  :CanopySnowEvap    CANEVP_ALL         CANOPY_SNOW     ATMOSPHERE
  :SnowBalance       SNOBAL_SIMPLE_MELT SNOW            SNOW_LIQ
    :-->Overflow     RAVEN_DEFAULT      SNOW_LIQ        PONDED_WATER
  :Flush             RAVEN_DEFAULT      PONDED_WATER    GLACIER
    :-->Conditional HRU_TYPE IS GLACIER
  :GlacierMelt       GMELT_HBV          GLACIER_ICE     GLACIER
  :GlacierRelease    GRELEASE_HBV_EC    GLACIER         SURFACE_WATER
  :Infiltration      INF_HBV            PONDED_WATER    MULTIPLE
  :Flush             RAVEN_DEFAULT      SURFACE_WATER   FAST_RESERVOIR
    :-->Conditional HRU_TYPE IS_NOT GLACIER
  :SoilEvaporation   SOILEVAP_HBV       SOIL[0]         ATMOSPHERE
  :CapillaryRise     CRISE_HBV          FAST_RESERVOIR 	SOIL[0]
  :LakeEvaporation   LAKE_EVAP_BASIC    SLOW_RESERVOIR  ATMOSPHERE
  :Percolation       PERC_CONSTANT      FAST_RESERVOIR 	SLOW_RESERVOIR
  :Baseflow          BASE_POWER_LAW     FAST_RESERVOIR  SURFACE_WATER
  :Baseflow          BASE_LINEAR        SLOW_RESERVOIR  SURFACE_WATER
:EndHydrologicProcesses
#
:AggregatedVariable FAST_RESERVOIR AllHRUs
:AggregatedVariable SLOW_RESERVOIR AllHRUs

#

",

    "HBV-Light"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0
#
# --Model Details -------------------------------
:Method         ORDERED_SERIES
:SoilModel      SOIL_MULTILAYER 3
:Routing        ROUTE_NONE
:CatchmentRoute ROUTE_TRI_CONVOLUTION
:Evaporation    PET_DATA
:RainSnowFraction RAINSNOW_HBV
:PotentialMeltMethod POTMELT_DEGREE_DAY
:OroTempCorrect OROCORR_HBV
:OroPrecipCorrect OROCORR_HBV
:OroPETCorrect OROCORR_HBV
:CloudCoverMethod CLOUDCOV_NONE
:PrecipIceptFract PRECIP_ICEPT_USER

# --Hydrologic Processes-------------------------
:Alias TOPSOIL SOIL[0]
:Alias FAST_RESERVOIR SOIL[1]
:Alias SLOW_RESERVOIR SOIL[2]

:HydrologicProcesses
  :SnowRefreeze   FREEZE_DEGREE_DAY   SNOW_LIQ        SNOW
  :Precipitation  PRECIP_RAVEN        ATMOS_PRECIP    MULTIPLE
  :SnowBalance    SNOBAL_SIMPLE_MELT  SNOW            SNOW_LIQ
  :-->Overflow    RAVEN_DEFAULT       SNOW_LIQ        PONDED_WATER
  :Infiltration   INF_HBV             PONDED_WATER    MULTIPLE
  :Flush          RAVEN_DEFAULT       SURFACE_WATER   FAST_RESERVOIR
  :SoilEvaporation SOILEVAP_HBV       TOPSOIL         ATMOSPHERE
  :CapillaryRise  RISE_HBV            FAST_RESERVOIR  TOPSOIL
  :Percolation    PERC_CONSTANT       FAST_RESERVOIR  SLOW_RESERVOIR
  :Baseflow       BASE_POWER_LAW      FAST_RESERVOIR  SURFACE_WATER
  :Baseflow       BASE_THRESH_POWER   FAST_RESERVOIR  SURFACE_WATER
  :Baseflow       BASE_LINEAR         SLOW_RESERVOIR  SURFACE_WATER
:EndHydrologicProcesses

#

",

"GR4J"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0

:Method              ORDERED_SERIES
:Interpolation       INTERP_NEAREST_NEIGHBOR

:Routing             ROUTE_NONE
:CatchmentRoute      ROUTE_DUMP

:Evaporation         PET_DATA
:RainSnowFraction    RAINSNOW_DINGMAN
:PotentialMeltMethod POTMELT_DEGREE_DAY
:OroTempCorrect      OROCORR_SIMPLELAPSE
:OroPrecipCorrect    OROCORR_SIMPLELAPSE

:SoilModel           SOIL_MULTILAYER  4

# --Hydrologic Processes-------------------------
:Alias PRODUCT_STORE      SOIL[0]
:Alias ROUTING_STORE      SOIL[1]
:Alias TEMP_STORE         SOIL[2]
:Alias GW_STORE           SOIL[3]
:HydrologicProcesses
 :Precipitation         PRECIP_RAVEN       ATMOS_PRECIP    MULTIPLE
 :SnowTempEvolve        SNOTEMP_NEWTONS    SNOW_TEMP
 :SnowBalance           SNOBAL_CEMA_NEIGE  SNOW            PONDED_WATER
 :OpenWaterEvaporation  OPEN_WATER_EVAP    PONDED_WATER    ATMOSPHERE
 :Infiltration          INF_GR4J           PONDED_WATER    MULTIPLE
 :SoilEvaporation       SOILEVAP_GR4J      PRODUCT_STORE   ATMOSPHERE
 :Percolation           PERC_GR4J          PRODUCT_STORE   TEMP_STORE
 :Flush                 RAVEN_DEFAULT      SURFACE_WATER   TEMP_STORE
 :Split          RAVEN_DEFAULT TEMP_STORE  CONVOLUTION[0]  CONVOLUTION[1] 0.9
 :Convolve              CONVOL_GR4J_1      CONVOLUTION[0]  ROUTING_STORE
 :Convolve              CONVOL_GR4J_2      CONVOLUTION[1]  TEMP_STORE
 :Percolation           PERC_GR4JEXCH      ROUTING_STORE   GW_STORE
 :Percolation           PERC_GR4JEXCH2     TEMP_STORE      GW_STORE
 :Flush                 RAVEN_DEFAULT      TEMP_STORE      SURFACE_WATER
 :Baseflow              BASE_GR4J          ROUTING_STORE   SURFACE_WATER
:EndHydrologicProcesses

#

    ", "CdnShield"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0

:Method              ORDERED_SERIES
:InterpolationMethod NEAREST_NEIGHBOR

:SoilModel           SOIL_MULTILAYER  3

:Routing             ROUTE_DIFFUSIVE_WAVE
:CatchmentRoute      ROUTE_TRI_CONVOLUTION
:Evaporation         PET_HARGREAVES_1985
:OW_Evaporation      PET_HARGREAVES_1985
:SWCanopyCorrect     SW_CANOPY_CORR_STATIC
:RainSnowFraction    RAINSNOW_DINGMAN
:PotentialMeltMethod POTMELT_DEGREE_DAY
:PrecipIceptFract    PRECIP_ICEPT_LAI

:MonthlyInterpolationMethod MONTHINT_LINEAR_MID

# :LakeStorage LAKE_STORAGE

# --Hydrologic Processes-------------------------
:Alias       SOIL0 SOIL[0]
:Alias       SOIL1 SOIL[1]
:Alias       SOIL2 SOIL[2]
:HydrologicProcesses
    :SnowRefreeze       FREEZE_DEGREE_DAY SNOW_LIQ      SNOW
    :Precipitation      PRECIP_RAVEN      ATMOS_PRECIP  MULTIPLE
    :CanopyEvaporation  CANEVP_MAXIMUM    CANOPY        ATMOSPHERE
    :CanopySnowEvap     CANEVP_MAXIMUM    CANOPY_SNOW   ATMOSPHERE
    :SnowBalance       	SNOBAL_TWO_LAYER  MULTIPLE      MULTIPLE
    :Abstraction        ABST_FILL         PONDED_WATER  DEPRESSION
    :OpenWaterEvaporation OPEN_WATER_EVAP DEPRESSION    ATMOSPHERE
    :Infiltration       INF_HBV           PONDED_WATER  MULTIPLE
    # :LakeRelease        LAKEREL_LINEAR    LAKE_STORAGE  SURFACE_WATER
    :Baseflow           BASE_POWER_LAW    SOIL1         SURFACE_WATER
    :Baseflow           BASE_POWER_LAW    SOIL2         SURFACE_WATER
    :Interflow          INTERFLOW_PRMS    SOIL0         SURFACE_WATER
    :Percolation        PERC_GAWSER       SOIL0         SOIL1
    :Percolation        PERC_GAWSER       SOIL1         SOIL2
    :SoilEvaporation    SOILEVAP_ROOT     SOIL0         ATMOSPHERE
:EndHydrologicProcesses

#

", "MOHYSE"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0

:Method                ORDERED_SERIES

:Routing               ROUTE_NONE
:CatchmentRoute        ROUTE_GAMMA_CONVOLUTION

:PotentialMeltMethod   POTMELT_DEGREE_DAY
:Evaporation           PET_MOHYSE
:RainSnowFraction      RAINSNOW_DATA
:DirectEvaporation

:SoilModel             SOIL_TWO_LAYER

:HydrologicProcesses
  :SoilEvaporation  SOILEVAP_LINEAR    SOIL[0]       ATMOSPHERE
  :SnowBalance      SNOBAL_SIMPLE_MELT SNOW          PONDED_WATER
  :Precipitation    RAVEN_DEFAULT      ATMOS_PRECIP  MULTIPLE
  :Infiltration     INF_HBV            PONDED_WATER  SOIL[0]
  :Baseflow         BASE_LINEAR        SOIL[0]       SURFACE_WATER
  :Percolation      PERC_LINEAR        SOIL[0]       SOIL[1]
  :Baseflow         BASE_LINEAR        SOIL[1]       SURFACE_WATER
:EndHydrologicProcesses

#

", "HMETS"="
:StartDate 1953-01-01  00:00:00
:EndDate   2009-12-31 00:00:00
:TimeStep   24:00:00

:PotentialMeltMethod     POTMELT_HMETS
:RainSnowFraction        RAINSNOW_DATA
:Evaporation             PET_OUDIN

:CatchmentRoute          ROUTE_DUMP
:Routing                 ROUTE_NONE

:SoilModel               SOIL_TWO_LAYER

:HydrologicProcesses
 :SnowBalance     SNOBAL_HMETS   MULTIPLE     MULTIPLE
 :Precipitation   RAVEN_DEFAULT  ATMOS_PRECIP MULTIPLE
 :Infiltration    INF_HMETS      PONDED_WATER MULTIPLE
   :Overflow      OVERFLOW_RAVEN SOIL[0]      CONVOLUTION[1]
 :Baseflow        BASE_LINEAR    SOIL[0]      SURFACE_WATER #interflow
 :Percolation     PERC_LINEAR    SOIL[0]      SOIL[1]       #recharge
   :Overflow      OVERFLOW_RAVEN SOIL[1]      CONVOLUTION[1]
 :SoilEvaporation SOILEVAP_ALL   SOIL[0]      ATMOSPHERE      #AET
 :Convolve        CONVOL_GAMMA   CONVOLUTION[0] SURFACE_WATER #surf. runoff
 :Convolve        CONVOL_GAMMA_2 CONVOLUTION[1] SURFACE_WATER #delay. runoff
 :Baseflow        BASE_LINEAR    SOIL[1]      SURFACE_WATER
:EndHydrologicProcesses

#

",

"HYPR"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0

# Model options
#------------------------------------------------------------------------
:CatchmentRoute      TRIANGULAR_UH

:Evaporation         PET_FROMMONTHLY
:OW_Evaporation      PET_FROMMONTHLY
:SWRadiationMethod   SW_RAD_DEFAULT
:LWRadiationMethod   LW_RAD_DEFAULT
:RainSnowFraction    RAINSNOW_HBV
:PotentialMeltMethod POTMELT_HBV
:PrecipIceptFract    PRECIP_ICEPT_USER
:MonthlyInterpolationMethod  MONTHINT_LINEAR_21
:SoilModel           SOIL_MULTILAYER 3

# Soil Layer Alias Definitions
#------------------------------------------------------------------------
:Alias       FAST_RESERVOIR SOIL[1]
:Alias       SLOW_RESERVOIR SOIL[2]


# Hydrologic process order for HYPR Emulation
#------------------------------------------------------------------------
:HydrologicProcesses
  :SnowRefreeze      FREEZE_DEGREE_DAY  SNOW_LIQ        SNOW
  :Precipitation     PRECIP_RAVEN       ATMOS_PRECIP    MULTIPLE
  :CanopyEvaporation CANEVP_ALL         CANOPY          ATMOSPHERE
  :CanopySnowEvap    CANEVP_ALL         CANOPY_SNOW     ATMOSPHERE
  :SnowBalance       SNOBAL_SIMPLE_MELT SNOW            PONDED_WATER
  :Infiltration      INF_HBV            PONDED_WATER    MULTIPLE
  :Flush             RAVEN_DEFAULT      SURFACE_WATER   PONDED_WATER
  :Abstraction       ABST_PDMROF        PONDED_WATER    DEPRESSION
  :Flush             RAVEN_DEFAULT      SURFACE_WATER   FAST_RESERVOIR
  :SoilEvaporation   SOILEVAP_HYPR      MULTIPLE        ATMOSPHERE
  :Baseflow          BASE_LINEAR        FAST_RESERVOIR  SURFACE_WATER
  :Baseflow          BASE_THRESH_STOR   FAST_RESERVOIR  SURFACE_WATER
:EndHydrologicProcesses

#

",

"HYMOD"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0

# Model options for HYMOD emulation
#------------------------------------------------------------------------
:Routing             ROUTE_NONE
:CatchmentRoute      ROUTE_RESERVOIR_SERIES

:Evaporation         PET_HAMON
:OW_Evaporation      PET_HAMON
:SWRadiationMethod   SW_RAD_NONE
:LWRadiationMethod   LW_RAD_NONE
:CloudCoverMethod    CLOUDCOV_NONE
:RainSnowFraction    RAINSNOW_THRESHOLD
:PotentialMeltMethod POTMELT_DEGREE_DAY
:PrecipIceptFract    PRECIP_ICEPT_NONE

:SoilModel           SOIL_MULTILAYER 2

#------------------------------------------------------------------------
# Hydrologic process order for HYMOD Emulation
#
:HydrologicProcesses
  :Precipitation     PRECIP_RAVEN       ATMOS_PRECIP    MULTIPLE
  :SnowBalance       SNOBAL_SIMPLE_MELT SNOW            PONDED_WATER
  :Infiltration      INF_PDM            PONDED_WATER    MULTIPLE
  #0.5 is the  HYMOD_ALPHA parameter
  :Flush             RAVEN_DEFAULT      SURFACE_WATER   SOIL[1]          0.5
  :SoilEvaporation   SOILEVAP_PDM       SOIL[0]         ATMOSPHERE
  :Baseflow          BASE_LINEAR        SOIL[1]         SURFACE_WATER
:EndHydrologicProcesses

#

",

"SAC-SMA"="
:StartDate    2000-01-01 00:00:00
:Duration     365
:TimeStep     1.0

# Model options for SAC-SMA emulation
#------------------------------------------------------------------------
:PotentialMeltMethod     POTMELT_DEGREE_DAY
:RainSnowFraction        RAINSNOW_DATA
:Evaporation             PET_DATA
:CatchmentRoute          ROUTE_DUMP
:Routing                 ROUTE_NONE 

:SoilModel               SOIL_MULTILAYER 7

:Alias UZ_T  SOIL[0]
:Alias UZ_F  SOIL[1]
:Alias LZ_T  SOIL[2]
:Alias LZ_PF SOIL[3]
:Alias LZ_PS SOIL[4]  

:HydrologicProcesses
  :SnowBalance           SNOBAL_SIMPLE_MELT   SNOW          PONDED_WATER
  :Precipitation         RAVEN_DEFAULT        ATMOS_PRECIP  MULTIPLE 
  :SoilEvaporation       SOILEVAP_SACSMA      MULTIPLE      ATMOSPHERE
  :SoilBalance           SOILBAL_SACSMA       MULTIPLE      MULTIPLE
  :OpenWaterEvaporation  OPEN_WATER_RIPARIAN  SURFACE_WATER ATMOSPHERE  
:EndHydrologicProcesses

#

"
)

  # open file for appending or truncating initially, depending on header
  if (writeheader) {
    fc <- file(filename, open="a+")
  } else {
    fc <- file(filename, open="w+")
  }

  writeLines(model_templates[[template_name]], con=fc)
  close(fc)

  return (TRUE)
}
