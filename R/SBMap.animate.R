#' Create Animated Subbasin Map
#'
#' Plots Raven custom output into an animated subbasin map for the specified date range.
#'
#'  @param shpfilename filename of shapefile containing HRU polygons, with one column inidicating Raven HRU ID
#'  @param subIDcol string of subbasin ID column in shapefile
#'  @param plot.daterange string of date range to create in GIF
#'  @param cust.data custom data set as read in by custom.read, for daily by_subbasin data
#'  @param leg.title text for legend title
#'  @param leg.pos position of legend
#'  @param normalize.data whether to normalize data by all cust.data (TRUE) or just the data for the given date (FALSE)
#'  @param colour1 string indicating which colour (text or code) to use on lower bound of range
#'  @param colour2 string indicating which colour (text or code) to use on upper bound of range
#'  @param num.classes number of classes to use in legend. Does not change the actual display colours
#'  @param invalid.stop whether to stop if invalid basins are found (TRUE) or just continue with a warning (FALSE)
#'  @param basins.label label to put on basins, one of c('None,'subID','value') to show nothing, subbasinIDs, or actual plotted values
#'  @param plot.title title across top of plot
#'  @param gif.filename fielname of outputted gif file
#'  @param transition.speed transition speed between plots in gif, in ms
#'
#' @details Requires \href{https://www.imagemagick.org/script/download.php}{ImageMagick} to be installed on your system
#'
#' @author Robert Chlumsky
#'
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{SBMap.plot}} to create a static subbasin map
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven web site}
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#'
#' # Warning: example not run
#'
#' \dontrun{
#' plot.daterange <- '2000-01-01/2000-01-31'
#' cust.data <- custom.read('PRECIP_Daily_Average_BySubbasin.csv')
#' leg.title <- 'Legend: Precip (mm/d)'
#' plot.title <- 'Precipitation (mm/d)'
#'
#' # create GIF of custom data plots
#' SBmap.plot.animate(shpfilename,subIDcol,plot.daterange,cust.data,plot.title=plot.title,leg.title=leg.title,gif.filename = 'Precip_mmd_Jan2000.gif')
#'
#' transition.speed <- 30
#' plot.daterange <- '2000-02-01/2000-02-28'
#' cust.data <- custom.read('SNOW_Daily_Average_BySubbasin.csv')
#' plot.title <- 'Snow Cover (mm)'
#' leg.title <- 'Legend: Snow (mm)'
#' colour1<-"green"
#' colour2<-"white
#' num.classes=7
#'
#' # create GIF of custom data plots
#' SBmap.plot.animate(shpfilename,subIDcol,plot.daterange,cust.data,plot.title=plot.title,colour.scheme=colour.scheme,
#'                    num.classes=num.classes,leg.title=leg.title,transition.speed = transition.speed,gif.filename = 'Snow_mm_Jan2000.gif')
#' }
#'
#' @export SBMap.animate
SBMap.animate <- function(shpfilename,subIDcol,plot.daterange,cust.data,leg.title='Legend',leg.pos='bottomleft',
                               normalize.data=TRUE,colour1="azure",colour2="white",
                               num.classes=5,invalid.stop=TRUE,basins.label='subID',plot.title='',gif.filename='subbasin_animated_plot.gif',
                               transition.speed=50) {

  current.wd <- getwd()
  rand.dir <- 'temp_make_SBplot_gif_4724'
  dir.create(rand.dir)

  # get the dates to plot from the supplied plot.daterange object
  plot.dates <- lubridate::date(cust.data[plot.daterange])

  png(file=paste0(rand.dir,"/","plot_%02d.png"), width=500, height=500)
  for (i in seq(1,length(plot.dates))) {
    SBMap.plot(shpfilename,subIDcol,plot.dates[i],cust.data,leg.title,leg.pos,
               normalize.data,colour1,colour2,
               num.classes,invalid.stop,basins.label,plot.title)
  }
  dev.off()

  # convert the .png files to one .gif file using ImageMagick.
  # The system() function executes the command as if it was done
  # in the terminal. the -delay flag sets the time between showing
  # the frames, i.e. the speed of the animation.
  # system("convert -delay 80 *.png example_1.gif")
  system(paste("convert -delay",as.character(transition.speed),paste0(rand.dir,"/","*.png"),gif.filename))

  # remove individual png files
  # file.remove(list.files(pattern=".png"))

  # delete subfolder
  unlink(rand.dir,recursive = T,force = T)
  return(TRUE)
}
