#' @title EC Climate Gauge Record Overlap Visualization
#'
#' @description
#' This function plots the length of Environment Canada climate station records,
#' accessed via the \pkg{weathercan} package, to identify periods in which multiple
#' station records overlap.
#'
#' @details
#' Accepts outputs from either the \code{stations_search()} or \code{weather_dl()} functions
#' from the \pkg{weathercan} package and extracts the start and end dates of the record
#' from each station for plotting.
#'
#' Station records are plotted chronologically on a timeline, and can be colored
#' according to either the station's elevation (default, works for both types of inputs)
#' or the station's distance from a point of interest (works only when supplying
#' \code{stations_search()} results as metadata input).
#'
#' The timeline plot is accompanied by a bar plot counting the number of stations with
#' available data year by year.
#'
#' Large differences in elevation between stations may point towards consideration for
#' the effect of lapse rates on climate forcings driving a model response.
#'
#' @param metadata tibble of the station meta-data from \code{weathercan::stations_search()}
#' @param stndata tibble of the station data from \code{weathercan::weather_dl()}
#' @param variables if using \code{weathercan::weather_dl()}, column names for variables of interest (currently only accepts 1 per call)
#' @param colorby column name by which to color station records. Set to 'elev' (elevation) by default. Can be set to
#' "dist" (distance from coordinates of interest) if supplying \code{weathercan::stations_search} results.
#'
#' @return returns a 2x1 plot object containing 2 ggplot objects
#'   \item{}{A chronological horizontal bar plot depicting each station's record period}
#'   \item{}{A vertical bar plot depicting the number of station records available each year}
#'
#' @examples
#' # load metadata from RavenR sample data
#' data(rvn_weathercan_metadata_sample)
#'
#' ## code that would be used to download metadata using weathercan
#' # library(weathercan)
#' #
#' # metadata = stations_search(coords=c(50.109,-120.787),
#' #    dist=150, # EC stations 150 km of Merritt, BC
#' #   interval='day'
#' # )
#' # metadata = metadata[metadata$start>=2000,] # subset stations with recent data
#' # metadata = metadata[1:3,] # take only the first 3 stations for brevity
#'
#' # plot line colours by station elevation
#' rvn_met_recordplot(metadata=rvn_weathercan_metadata_sample, colorby='elev')
#'
#' # plot line colours by distance to specified co-ordinates
#' rvn_met_recordplot(metadata=rvn_weathercan_metadata_sample, colorby='distance')
#'
#' ## load sample weathercan::weather_dl() downloaded data with a single station for a basic analysis of period of record
#' data(rvn_weathercan_sample)
#'
#' # compare records for a specific variable
#' rvn_met_recordplot(stndata=rvn_weathercan_sample,variables = "total_precip")
#'
#' @importFrom ggplot2 geom_point geom_rect geom_col xlab ylab ggtitle scale_x_continuous scale_color_continuous theme_bw theme
#' @importFrom cowplot plot_grid
#' @export rvn_met_recordplot
#'
rvn_met_recordplot <- function(metadata=NULL,stndata=NULL,variables=NULL,colorby=NULL){
  if(!is.null(metadata) & !is.null(stndata)){
    stop('Please supply either one of the outputs from weathercan::weather_dl() OR weathercan::stations_search(), not both')}

  if(!is.null(stndata) & is.null(variables)){
    stop('Please specify which variable you would like to view record lengths for')
  }

  if(!is.null(stndata) & !is.null(variables)){
    stndata = stndata[,c('station_name','station_id','year','elev',variables)] # subset only the variable of interest

    # extract start/end years for specified variables where data actually exists
    metadata = do.call('rbind',lapply(unique(stndata$station_id),function(sid){
      onestation = stndata[stndata$station_id==sid,]
      metadata = data.frame(station_name=unique(onestation$station_name),
                             start=as.numeric(min(onestation$year[!is.na(onestation[,variables])])),
                             end = as.numeric(max(onestation$year[!is.na(onestation[,variables])])),
                             elev= unique(onestation$elev[onestation$station_id==sid]))
      return(metadata)
    }))
  }

  if(!is.null(metadata)){
    # reorder chronologically by start of record
    metadata$station_name=factor(metadata$station_name,levels=metadata$station_name[order(-metadata$start)])

    # plot records to show overlap
    xmax = max(metadata$end)
    xmin = min(metadata$start)

    if(is.null(variables)){variables = 'overall station records'} # plot title

    if(!is.null(colorby)){                                        # coloring parameter
      if(sum(colnames(metadata)==colorby)<1){                     # Check if parameter exists in generated metadata dataframe.

        stop("Coloring parameter not found. Please specify either 'elev' or 'distance'.
           Note that 'distance' only works when supplying weathercan::stations_search results.")

        }else if(colorby=='distance'){
          colorname = 'distance from point of interest [km]'
        }else if(colorby=='elev'){
         colorby = 'elev'
         colorname = 'elevation [m]'
        }
    }else{                                                        # if no input, default coloring is by elevation
      colorby='elev'
      colorname='elevation [m]'
    }

    overlapPlot <- ggplot(data=metadata)+
      geom_point(aes(x=start,y=station_name))+
      geom_point(aes(x=end,y=station_name))+
      geom_rect(aes(xmin=start,xmax=end,ymin=station_name,ymax=station_name,color=get(colorby)))+
      ggtitle('climate station record periods',subtitle = variables)+
      scale_color_continuous(name=colorname, high = "#56B4E9", low = "#D55E00")+
      scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,1))+
      theme_bw()+
      theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust = 0.5),
            legend.position = 'bottom',axis.title.y=element_blank())
  }

  # Count number of station records available for each year
  year = min(metadata$start):max(metadata$end)
  recordsum = do.call('rbind',lapply(year,function(y){
    n = sum(metadata$start <= y & metadata$end >= y)
    return(data.frame(year=y,nstations=n))
  }))

  # plot station count per year
  stncount = ggplot(recordsum,aes(x=year,y=nstations))+
    geom_col()+
    ggtitle('Data availablity by year')+
    xlab('year')+
    ylab('stations')+
    scale_x_continuous(breaks=seq(xmin,xmax,1))+
    theme_bw()+
    theme(plot.title=element_text(hjust = 0.5))

  out <- plot_grid(overlapPlot,stncount,nrow=2 )
  return(out)
}
