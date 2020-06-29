#' Create Animated Subbasin Map
#'
#' Plots Raven custom output into an animated subbasin map for the specified date range.
#'
#' @param shpfilename filename of shapefile containing HRU polygons, with one column inidicating Raven HRU ID
#' @param subIDcol string of subbasin ID column in shapefile
#' @param plot.daterange string of date range to create in GIF
#' @param cust.data custom data set as read in by custom.read, for daily by_subbasin data
#' @param leg.title text for legend title
#' @param leg.pos position of legend
#' @param normalize.data whether to normalize data by all cust.data (TRUE) or just the data for the given date (FALSE)
#' @param colour1 string indicating which colour (text or code) to use on lower bound of range
#' @param colour2 string indicating which colour (text or code) to use on upper bound of range
#' @param num.classes number of classes to use in legend. Does not change the actual display colours
#' @param invalid.stop whether to stop if invalid basins are found (TRUE) or just continue with a warning (FALSE)
#' @param basins.label label to put on basins, one of c('None,'subID','value') to show nothing, subbasinIDs, or actual plotted values
#' @param plot.title title across top of plot
#' @param plot.invalid boolean indicating whether to plot invalid basins in grey (currently disabled)
#' @param gif.filename filename of outputted gif file
#' @param gif.speed time in seconds between images
#' @param cleanup boolean indicates whether to remove the scratch directory with image files stored.
#'
#' @details Requires \href{https://www.imagemagick.org/script/download.php}{ImageMagick} to be installed on your system
#' This function has been updated to make use of the magick R package, which makes a cleaner command stack for generating gifs.
#'
#' The cleanup parameter can be used to disable removal of the scratch directory where individual plot images are stored, which
#' may be desired if the user wishes to keep individual plots in addition to the .gif file created by this function.
#'
#' Note that the normalize.data parameter is default as TRUE for this function. In generating a series of plots the data should
#' ideally be normalized across the entire dataset for consistency between frames, however, this is not the default for the
#' static plot call.
#'
#' @author Robert Chlumsky
#'
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @importFrom sf read_sf st_centroid st_coordinates
#' @importFrom magick image_read image_write_gif
#' @importFrom stats runif
#' @importFrom grDevices png dev.off
#'
#' @seealso \code{\link{SBMap.plot}} to create a static subbasin map
#'
#' See also the \href{http://raven.uwaterloo.ca/}{Raven web site}
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' #' # shapefile of Nith basin
#' shpfilename <- system.file("extdata","Nith_shapefile_sample.shp",package="RavenR")
#'
#' # Custom Output data from Raven for Nith basin
#' cust.data <- custom.read(system.file("extdata","run1_PRECIP_Daily_Maximum_BySubbasin.csv",
#'                                      package="RavenR"))
#'
#' subIDcol <- 'subID' # attribute in shapefile with subbasin IDs
#' leg.title <- 'Legend - Precip (mm)'
#' colour1 <- "white"
#' colour2 <- "blue"
#' num.classes <- 5
#' plot.title <- 'Daily Max Precip. (mm)'
#' plot.daterange <- '2003-05-01/2003-06-30'
#'
#' gif.filename='Nith_precip_May2003_June2003.gif'
#' gif.speed <- 0.5
#' cleanup <- FALSE
#'
#' SBMap.animate(shpfilename,subIDcol,plot.daterange,cust.data,plot.title=plot.title,
#'               colour1 = colour1, colour2=colour2,
#'               leg.title = leg.title, normalize=T, num.classes=num.classes,
#'               gif.filename=gif.filename,
#'               gif.speed=gif.speed, cleanup=cleanup
#' )
#'
#' @export SBMap.animate
SBMap.animate <- function(shpfilename,subIDcol,plot.daterange,cust.data,leg.title='Legend',leg.pos='bottomleft',
                               normalize.data=TRUE,colour1="azure",colour2="white",
                               num.classes=5,invalid.stop=TRUE,basins.label='subID',plot.title='',plot.invalid=F,
                               gif.filename='subbasin_animated_plot.gif',
                               gif.speed=1,
                               cleanup=T) {

  current.wd <- getwd()
  rand.dir <- paste0('scratch_SBMap',"_",paste0(ceiling(runif(20,min=0,max=10)),collapse=""))
  dir.create(rand.dir)

  # get the dates to plot from the supplied plot.daterange object
  plot.dates <- lubridate::date(cust.data[plot.daterange])

  for (i in seq(1,length(plot.dates))) {
    png(file=paste0(rand.dir,"/",sprintf("plot_%02d.png",i)), width=500, height=500)
    SBMap.plot(shpfilename,subIDcol,plot.dates[i],cust.data,leg.title,leg.pos,
               normalize.data,colour1,colour2,
               num.classes,invalid.stop,basins.label,plot.title, plot.invalid)
    dev.off()
  }
  try(dev.off(), silent=TRUE)

  # ---- OLD METHOD BELOW
  # convert the .png files to one .gif file using ImageMagick.
  # The system() function executes the command as if it was done
  # in the terminal. the -delay flag sets the time between showing
  # the frames, i.e. the speed of the animation.
  # system("convert -delay 80 *.png example_1.gif")
  # system(paste("convert -delay",as.character(transition.speed),paste0(rand.dir,"/","*.png"),gif.filename))
  # ------------


  # generate an animation from the images created

  pp <- list.files(pattern='*\\.png', recursive=TRUE)
  pp <- pp[grep(rand.dir,pp)]
  img <- image_read(pp)
  image_write_gif(img, path=gif.filename, delay=gif.speed)

  # delete subfolder
  if (cleanup) { unlink(rand.dir,recursive = T,force = T) }

  return(TRUE)
}
