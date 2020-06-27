#' Plots summary of watershed forcing functions
#'
#' rvn_forcings_plot generates a set of 5 plots (precip,temperature,PET,radiation,
#' and potential melt), which summarize the watershed-averaged forcings. Returns a list with the individual plots. 
#'
#' This function creates multiple plots from a ForcingFunctions.csv file
#' structure generating using RavenR's forcings.read function
#'
#' @param forcings forcings attribute from forcings.read function
#' @param prd (optional) time period over which the plots are generated
#' @seealso \code{\link{rvn_forcings_read}} for the function used to read in the
#' forcings function data
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven forcing plot diagnostics
#' @examples
#'
#' # read in sample forcings data
#' data("rvn_forcing_data")
#' fdata <- rvn_forcing_data$forcings
#'
#' # plot forcings data
#' rvn_forcings_plot(fdata)
#'
#' plot subset of forcing data for 2002-2003 water year
#' prd = "2002-10-01/2003-09-30"
#' rvn_forcings_plot(fdata,prd)
#'
#' @export rvn_forcings_plot
rvn_forcings_plot <-function(forcings,prd=NULL){

  require(cowplot)
  
  plot.data <- fortify(forcings)
  
  # Precipitation
  plot.data$Total_Precip <- plot.data$rain + plot.data$snow
  precip.data <- reshape::melt(plot.data, id.vars = "Index", measure.vars = c("Total_Precip","snow"))
  p1 <- ggplot(precip.data)+
    geom_line(aes(x= Index, y= value, color = variable))+
    scale_color_manual(values = c("blue", "cyan"))+
    ylim(c(0,max(plot.data$Total_Precip)))+
    ylab("Precipitation (mm/d)")+
    xlab("")+
    theme_bw()+
    theme(legend.position = c(0.8,0.8),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          axis.title = element_text(size = 7))
  
  
  #Temperature
  temp.data <- reshape::melt(plot.data, id.vars = "Index", measure.vars = c("temp_daily_max", "temp_daily_min"))
  temp.data$color <- "red"
  temp.data$color[temp.data$value<0] <- "purple"
  
  
  p2 <- ggplot(temp.data)+
    geom_line(aes(x= Index, y= value, group = variable, color = color))+
    geom_hline(yintercept = 0)+
    scale_color_manual(values = c("red","purple"))+
    ylim(c(min(plot.data$temp_daily_min),max(plot.data$temp_daily_max)))+
    ylab(expression(paste("Min/Max Daily Temperature (",degree,"C)")))+
    xlab("")+
    theme_bw()+
    theme(legend.position = "none",
          axis.title = element_text(size = 7))
  
  #PET 
  p3 <- ggplot(plot.data)+
    geom_line(aes(x = Index, y = PET), color = "navy")+
    ylab('PET (mm/d)')+
    xlab("")+
    theme_bw()+
    theme(legend.position = "none",
          axis.title = element_text(size = 7))
  
  #Radiation 
  plot.data$SW_LW <- plot.data$SW.radiation + plot.data$LW.radiation
  rad.data <- reshape::melt(plot.data, id.vars = "Index", measure.vars = c("LW.radiation","SW.radiation","ET.radiation","SW_LW"))
  
  p4 <- ggplot(rad.data)+
    geom_line(aes(x = Index, y = value, color = variable))+
    scale_color_manual(values = c("black", "blue", "blue", "blue"))+
    ylab('Radiation (MJ/m2/d)')+
    xlab("")+
    theme_bw()+
    theme(legend.position = "none",
          axis.title = element_text(size = 7))
  
  
  # Potential melt
  p5 <- ggplot(plot.data)+
    geom_line(aes(x = Index, y = potential.melt), color = "navy")+
    ylab('Potential Melt (mm/d)')+
    xlab("")+
    theme_bw()+
    theme(legend.position = "none",
          axis.title = element_text(size = 7))
  
  #Plot Title
  ts=forcings$snow
  N <- nrow(ts)
  titl <- sprintf("Watershed-averaged Forcings (%d-%02d-%02d to %i-%02d-%02d)",year(ts[1,1]),month(ts[1,1]),day(ts[1,1]),year(ts[N,1]),month(ts[N,1]),day(ts[N,1]) )
  
  
  # Change period if required
  if (!(is.null(prd))){
    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
    
    p1 <- p1 +
      scale_x_datetime(limits = c(as.POSIXct(firstsplit[1]), as.POSIXct(firstsplit[2])))
    
    p2 <- p2 +
      scale_x_datetime(limits = c(as.POSIXct(firstsplit[1]), as.POSIXct(firstsplit[2])))
    
    p3 <- p3 +
      scale_x_datetime(limits = c(as.POSIXct(firstsplit[1]), as.POSIXct(firstsplit[2])))
    
    p4 <- p4 +
      scale_x_datetime(limits = c(as.POSIXct(firstsplit[1]), as.POSIXct(firstsplit[2])))
    
    p5 <- p5 +
      scale_x_datetime(limits = c(as.POSIXct(firstsplit[1]), as.POSIXct(firstsplit[2])))
    
    ts=forcings$snow[prd]
    N <- nrow(ts)
    titl <- sprintf("Watershed-averaged Forcings (%d-%02d-%02d to %i-%02d-%02d)",year(ts[1,1]),month(ts[1,1]),day(ts[1,1]),year(ts[N,1]),month(ts[N,1]),day(ts[N,1]) )
    
  }
  
  #Add Title and Create 1 Plot
  title <- ggdraw() + 
    draw_label(titl, x = 0.5, hjust = 0.5)
  all_forcing_plots <- plot_grid(title,p1,p2,p3,p4,p5,ncol = 1, rel_heights = c(0.1,1,1,1,1,1))
  
  plot(all_forcing_plots)
  
  forcing_plots <- list("Precipitation" = p1, 
                        "Temperature" = p2,
                        "PET" = p3,
                        "Radiation" = p4,
                        "PotentialMelt" = p5,
                        "All Forcings" = all_forcing_plots)
  
  return(forcing_plots)
}
