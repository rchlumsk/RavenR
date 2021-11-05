#' @title Run Raven Executable
#'
#' @description
#' Invokes shell to execute a Raven model.
#'
#' @details
#' Uses the \code{\link{shell}} command to run the Raven.exe command.
#'
#' If the \code{fileprefix} is not supplied, the function will detect the rvi file automatically and
#' use that in the run (if exactly one rvi file exists in the directory).
#'
#' The \code{indir} path must point to the main Raven input files, if they are not in the working directory.
#'
#' The \code{ravenexe} must point to the Raven.exe file; if not supplied it will look for it in the RavenR/extdata
#' path that is saved to when using \code{\link{rvn_download}}.
#'
#' @param fileprefix file prefix for main Raven input files.
#' @param indir string path for Raven input files
#' @param ravenexe file path to Raven executable
#' @param outdir string path for Raven output files (optional)
#' @param rvc file path to specific rvc file (optional)
#' @param rvt file path to specific rvt file (optional)
#' @param rvp file path to specific rvp file (optional)
#' @param rvh file path to specific rvh file (optional)
#' @param showoutput boolean whether to show output in console (passed to show.output.on.console within system) (default FALSE)
#'
#' @return Returns output code from the system command when running Raven
#'
#' @seealso \code{\link{rvn_download}}
#'
#' @examples
#' \dontrun{
#' ## NOT RUN (data download + runs Raven.exe)
#' url<-"http://raven.uwaterloo.ca/files/RavenTutorialFiles.zip"
#' temploc <- tempdir()
#' destfile<-paste(temploc,"/RavenTutorialFiles.zip",sep="")
#' download.file(url,destfile)
#' destdir<-paste(temploc,"/RavenTutorialFiles",sep="")
#' dir.create(destdir)
#' unzip(zipfile=destfile,exdir=destdir)
#' file.remove(destfile)
#'
#' # Irondeqoiut example
#' rvn_run(indir=paste(destdir,"/Irond",sep=""),
#'         showoutput=TRUE)
#' }
#'
#' @export rvn_run
rvn_run <- function(fileprefix=NULL, indir=getwd(), ravenexe=NULL,
                  outdir=NULL,
                  rvc=NULL, rvt=NULL, rvp=NULL, rvh=NULL,
                  showoutput=FALSE) {

   if (is.null(fileprefix)) {
      fileprefix<-gsub(".rvi","",list.files(indir,pattern=".rvi"))
      if(length(fileprefix)==0) stop("No fileprefix supplied and no rvi file found in the input directory!")
      if(length(fileprefix)>1) stop("No fileprefix supplied and multiple rvi files found in the input directory!")
   }

   if (is.null(ravenexe)) {
      if (file.exists(paste0(system.file("extdata",package="RavenR"),"/Raven.exe"))) {
         ravenexe <- paste0(system.file("extdata",package="RavenR"),"/Raven.exe")
      } else{
         stop("No Raven.exe file path was supplied and none can be found in RavenR/extdata. Considering running `rvn_download()` to download the latest version of Raven for your system.")
      }
   }

   if (!dir.exists(indir)) {
      stop(sprintf("indir path of %s does not exist",indir))
   }

   if (!file.exists(file.path(indir,paste0(fileprefix,".rvi")))) {
      stop(sprintf("rvi file not found: %s",file.path(indir,paste0(fileprefix,".rvi")) ))
   }

   # shell/system functions are vulnerable to paths with many characters or spaces
   # here these paths are shortened: inputdir/ravenexe
   if(Sys.info()["sysname"]=="Windows")
   {
     ravenexe<-shortPathName(ravenexe)
     indir<-shortPathName(indir)
   }

   # build up RavenCMD
   RavenCMD <- sprintf("%s %s",
                       ravenexe, file.path(indir,fileprefix))

   if (!is.null(outdir)) {
      RavenCMD <- sprintf("%s -o %s", RavenCMD, outdir)
   }

   if (!is.null(rvc)) {
      if (!file.exists(rvc)) stop("Supplied rvc file does not exist")
      RavenCMD <- sprintf("%s -c %s", RavenCMD, rvc)
   }

   if (!is.null(rvt)) {
      if (!file.exists(rvt)) stop("Supplied rvt file does not exist")
      RavenCMD <- sprintf("%s -t %s", RavenCMD, rvt)
   }

   if (!is.null(rvp)) {
      if (!file.exists(rvp)) stop("Supplied rvp file does not exist")
      RavenCMD <- sprintf("%s -p %s", RavenCMD, rvp)
   }

   if (!is.null(rvh)) {
      if (!file.exists(rvh)) stop("Supplied rvh file does not exist")
      RavenCMD <- sprintf("%s -h %s", RavenCMD, rvh)
   }
   if(Sys.info()["sysname"]=="Windows")
   {
     res <- invisible(system(RavenCMD, show.output.on.console = showoutput))
   }else{
     res <- system(RavenCMD)
   }
   return(res)
}
