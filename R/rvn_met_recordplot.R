#' @title EC Climate Gauge Record Overlap Visualization
#'
#' @description
#' This function plots the length of Environment Canada climate station records,
#' accessed via the weathercan package, to identify periods in which multiple
#' station records overlap.
#'
#' @details
#' Accepts outputs from either the stations_search() or weather_dl() functions
#' from the weathercan package and extracts the start and end dates of the record
#' for plotting.
#'
#' @param metadata tibble of the station meta-data from weathercan::stations_search()
#' @param stndata tibble of the station data from weathercan::weather_dl()
#' @param variables if using weathercan::weather_dl(), column names for variables of interest (currently only accepts 1 per call)
#'
#' @examples
#' # Plot record lengths using outputs from weathercan::stations_search()
#' library(weathercan)
#' metadata = stations_search(coords=c(50.109,-120.787),interval='day',dist=150) # EC stations 150 km of Merritt, BC
#' metadata = metadata[metadata$start>=2000,] # subset stations with recent data
#' metadata = metadata[1:3,] # take only the first 3 stations for brevity
#'
#' rvn_met_recordplot(metadata=metadata)
#'
#' # Plot record lengths using outputs from weathercan::weather_dl()
#' library(weathercan)
#' ids = c(47408,51598,44927) # station_id for 3 EC stations used above
#' stndata=weather_dl(station_ids=ids,interval='day')
#'
#' rvn_met_recordplot(stndata=stndata,variables = "total_precip") # compare records for a specific variable
#'
#' @importFrom ggplot2
#' @export rvn_met_recordplot
rvn_met_recordplot <- function(metadata=NULL,stndata=NULL,variables=NULL){
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

    if(is.null(variables)){variables = 'overall station records'}
    overlapPlot = ggplot(data=metadata)+
      geom_point(aes(x=start,y=station_name))+geom_point(aes(x=end,y=station_name))+
      geom_rect(aes(xmin=start,xmax=end,ymin=station_name,ymax=station_name,color=elev))+
      ggtitle('climate station record periods',subtitle = variables)+
      scale_color_continuous(name='elevation [m]', high = "#56B1F7", low = "#132B43")+
      scale_x_continuous(limits=c(xmin,xmax),breaks=seq(xmin,xmax,1))+theme_bw()+
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
  stncount = ggplot(recordsum,aes(x=year,y=nstations))+geom_col()+
    ggtitle('Data availablity by year')+xlab('year')+ylab('stations')+
    scale_x_continuous(breaks=seq(xmin,xmax,1))+
    theme_bw()+theme(plot.title=element_text(hjust = 0.5))

  out = gridExtra::grid.arrange(overlapPlot,stncount, nrow=2)

  return(out)
}
