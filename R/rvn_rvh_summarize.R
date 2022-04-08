#' @title Summarize RVH object
#'
#' @description
#' Summarizes the RVH object provided in a number of useful ways, and returns a list with the summarized information.
#'
#' @param rvh rvh object as returned by \code{\link{rvn_rvh_read}} or \code{\link{rvn_rvh_query}}
#' @param return_list boolean whether the to return the summary list object, if \code{FALSE} only \code{TRUE} is returned (default \code{TRUE})
#'
#' @return Returns \code{TRUE} if the parameter \code{return_list} is \code{FALSE}, otherwise returns a list with multiple items:
#'  \item{total_subbasin_area}{total subbasin area, computed as the sum of all areas in \code{rvh$SBtable$Area}}
#'  \item{num_subbasins}{total number of subbasins}
#'  \item{num_hrus}{total number of HRUs}
#'  \item{num_unique_hrus}{total number of unique HRUs, i.e. HRUs with unique combination of LandUse, Vegetation, Soil Profile, and Terrain/Aquifer if relevant (see details)}
#'  \item{hru_summary_landuse}{a dataframe summarizing the RVH object by LandUse class}
#'  \item{hru_summary_vegetation}{a dataframe summarizing the RVH object by Vegetation class}
#'  \item{hru_summary_soilprofile}{a dataframe summarizing the RVH object by Soil Profile}
#'  \item{hru_summary_terrain}{a dataframe summarizing the RVH object by Terrain class}
#'  \item{hru_summary_aquifer}{a dataframe summarizing the RVH object by Aquifer class}
#'  \item{hru_summary_general}{a dataframe summarizing the RVH object by LandUse, Vegetation, Soil Profile, and Terrain/Aquifer if relevant (see details)}
#'  \item{dom_landuse}{dominant LandUse class by total area}
#'  \item{dom_landuse_ratio}{ratio of the total area of dominant LandUse class by total subbasin area}
#'  \item{dom_vegetation}{dominant Vegetation class by total area}
#'  \item{dom_vegetation_ratio}{ratio of the total area of dominant Vegetation class by total subbasin area}
#'  \item{dom_soilprofile}{dominant Soil Profile by total area}
#'  \item{dom_soilprofile_ratio}{ratio of the total area of dominant Soil Profile by total subbasin area}
#'  \item{dom_terrain}{dominant Terrain class by total area (NULL if no Terrain classes defined)}
#'  \item{dom_terrain_ratio}{ratio of the total area of dominant Terrain class by total subbasin area (NA if no Terrain classes defined)}
#'  \item{dom_aquifer}{dominant Aquifer class by total area (NULL if no Aquifer classes defined)}
#'  \item{dom_aquifer_ratio}{ratio of the total area of dominant Aquifer class by total subbasin area (NA if no Aquifer classes defined)}
#'
#' @details
#' The total subbasin area is the total area of all subbasins in the RVH object. If there are multiple outlets
#' in the model, or additional subbasin information that is not part of the model domain, this will be counted in the total
#' area and other summary diagnostics. Consider using \code{\link{rvn_rvh_query}} to isolate portions of the domain
#' in such instances.
#'
#' Information for dominant Terrain and Aquifer classes is also returned, but only if there are Terrain or Aquifer
#' classes respectively other than '[NONE]'. The number of unique HRUs include Terrain and Aquifer classes in the consideration of unique HRUs if
#' there are instances of classes other than '[NONE]' for these classes.
#'
#' @seealso
#' \code{\link{rvn_rvh_read}} to read a Raven RVH file into R
#' \code{\link{rvn_rvh_query}} to query the RVH file prior to other operations, such as summarizing)
#'
#' @examples
#' # load example rvh file
#' nith <- system.file("extdata","Nith.rvh",package = "RavenR")
#' rvh <- rvn_rvh_read(nith)
#'
#' # summarize rvh
#' rvn_rvh_summarize(rvh, return_list=FALSE)
#'
#' # query of HRUs upstream of basin 39, then summarize
#' rvh %>%
#'   rvn_rvh_query(subbasinID=39, condition="upstream_of") %>%
#'   rvn_rvh_summarize(return_list=FALSE)
#'
#' @export rvn_rvh_summarize
#' @importFrom dplyr as_tibble group_by summarise
#' @importFrom crayon green
rvn_rvh_summarize <- function(rvh=NULL, return_list=TRUE)
{

  # error checking
  if (is.null(rvh) | is.null(rvh$SBtable)| is.null(rvh$HRUtable) | is.null(rvh$SBnetwork) ) {
    stop("rvn_rvh_summarize: valid rvh object is required")
  }

  # create HRU summary data frames
  hru_summary_landuse <- rvh$HRUtable %>%
    as_tibble() %>%
    group_by(LandUse) %>%
    summarise(Total_Area=sum(Area), Avg_Elevation=mean(Elevation), Avg_Slope=mean(Slope), Avg_Aspect=mean(Aspect) )

  hru_summary_vegetation <- rvh$HRUtable %>%
    as_tibble() %>%
    group_by(Vegetation) %>%
    summarise(Total_Area=sum(Area), Avg_Elevation=mean(Elevation), Avg_Slope=mean(Slope), Avg_Aspect=mean(Aspect) )

  hru_summary_soilprofile <- rvh$HRUtable %>%
    as_tibble() %>%
    group_by(SoilProfile) %>%
    summarise(Total_Area=sum(Area), Avg_Elevation=mean(Elevation), Avg_Slope=mean(Slope), Avg_Aspect=mean(Aspect) )

  if (unique(rvh$HRUtable$Terrain) != c("[NONE]")){
    hru_summary_terrain <- rvh$HRUtable %>%
      as_tibble() %>%
      group_by(Terrain) %>%
      summarise(Total_Area=sum(Area), Avg_Elevation=mean(Elevation), Avg_Slope=mean(Slope), Avg_Aspect=mean(Aspect) )
  } else {
    hru_summary_terrain <- NULL
  }

  if (unique(rvh$HRUtable$Aquifer) != c("[NONE]")){
    hru_summary_aquifer <- rvh$HRUtable %>%
      as_tibble() %>%
      group_by(Aquifer) %>%
      summarise(Total_Area=sum(Area), Avg_Elevation=mean(Elevation), Avg_Slope=mean(Slope), Avg_Aspect=mean(Aspect) )
  } else {
    hru_summary_aquifer <- NULL
  }

  # hru_summary_general
  summarycols <- c("LandUse", "Vegetation","SoilProfile")
  if (unique(rvh$HRUtable$Terrain) != c("[NONE]")){
    summarycols <- c(summarycols, "Terrain")
  }
  if (unique(rvh$HRUtable$Aquifer) != c("[NONE]")){
    summarycols <- c(summarycols, "Aquifer")
  }

  hru_summary_general <- rvh$HRUtable %>%
    as_tibble() %>%
    group_by(across(all_of(summarycols))) %>%
    summarise(Total_Area=sum(Area), Avg_Elevation=mean(Elevation), Avg_Slope=mean(Slope), Avg_Aspect=mean(Aspect) )

  # dominant landuse, soilprofile, vegetation
  dom_landuse <- hru_summary_landuse[hru_summary_landuse$Total_Area == max(hru_summary_landuse$Total_Area),]
  dom_vegetation <- hru_summary_vegetation[hru_summary_vegetation$Total_Area == max(hru_summary_vegetation$Total_Area),]
  dom_soilprofile <- hru_summary_soilprofile[hru_summary_soilprofile$Total_Area == max(hru_summary_soilprofile$Total_Area),]

  ## create return list
  rl <- list(
    "total_subbasin_area"=sum(rvh$SBtable$Area),
    "num_subbasins"=nrow(rvh$SBtable),
    "num_hrus" =nrow(rvh$HRUtable),
    "num_unique_hrus" =nrow(hru_summary_general),
    "hru_summary_landuse"=hru_summary_landuse,
    "hru_summary_vegetation"=hru_summary_vegetation,
    "hru_summary_soilprofile"=hru_summary_soilprofile,
    "hru_summary_terrain"=hru_summary_terrain,
    "hru_summary_aquifer"=hru_summary_aquifer,
    "hru_summary_general"=hru_summary_general,
    "dom_landuse"=dom_landuse$LandUse,
    "dom_landuse_ratio"=dom_landuse$Total_Area/sum(rvh$SBtable$Area),
    "dom_vegetation"=dom_vegetation$Vegetation,
    "dom_vegetation_ratio"=dom_vegetation$Total_Area/sum(rvh$SBtable$Area),
    "dom_soilprofile"=dom_soilprofile$SoilProfile,
    "dom_soilprofile_ratio"=dom_soilprofile$Total_Area/sum(rvh$SBtable$Area)
  )

  # dominant terrain and aquifer class (append to list if valid)
  if (unique(rvh$HRUtable$Terrain) != c("[NONE]")){
    dom_terrain <- hru_summary_terrain[hru_summary_terrain$Total_Area == max(hru_summary_terrain$Total_Area),]
    rl <- append(rl,
                 values=list("dom_terrain"=dom_terrain$Terrain,
                          "dom_terrain_ratio"=dom_terrain$Total_Area/sum(rvh$SBtable$Area)))
  } else {
    rl <- append(rl,
                 values=list("dom_terrain"=NULL,
                          "dom_terrain_ratio"=NA))
  }

  if (unique(rvh$HRUtable$Aquifer) != c("[NONE]")){
    dom_aquifer <- hru_summary_aquifer[hru_summary_aquifer$Total_Area == max(hru_summary_aquifer$Total_Area),]
    rl <- append(rl,
                 values=list("dom_aquifer"=dom_aquifer$Aquifer,
                          "dom_aquifer_ratio"=dom_aquifer$Total_Area/sum(rvh$SBtable$Area)))
  } else {
    rl <- append(rl,
                 values=list("dom_aquifer"=NULL,
                          "dom_aquifer_ratio"=NA))
  }

  ## output messages summarizing information
  message(crayon::green(sprintf("Total Subbasin Area: %.2f (km^2)", rl$total_subbasin_area)))
  message(crayon::green(sprintf("Number of Subbasins: %i", rl$num_subbasins)))
  message(crayon::green(sprintf("Number of HRUs: %i", rl$num_hrus)))
  message(crayon::green(sprintf("Number of unique HRUs: %i", rl$num_unique_hrus)))
  message(crayon::green(sprintf("Dominant LandUse class: %s (%.2f%% of total subbasin area)", rl$dom_landuse, rl$dom_landuse_ratio*100)))
  message(crayon::green(sprintf("Dominant Vegetation class: %s (%.2f%% of total subbasin area)", rl$dom_vegetation, rl$dom_vegetation_ratio*100)))
  message(crayon::green(sprintf("Dominant Soil Profile: %s (%.2f%% of total subbasin area)", rl$dom_soilprofile, rl$dom_soilprofile_ratio*100)))
  if (!is.null(rl$dom_terrain)) {
    message(crayon::green(sprintf("Dominant Terrain class: %s (%.2f%% of total subbasin area)", rl$dom_terrain, rl$dom_terrain_ratio*100)))
  }
  if (!is.null(rl$dom_aquifer)) {
    message(crayon::green(sprintf("Dominant Aquifer class: %s (%.2f%% of total subbasin area)", rl$dom_aquifer, rl$dom_aquifer_ratio*100)))
  }

  if (return_list) {
    return(rl)
  } else {
    return(TRUE)
  }
}
