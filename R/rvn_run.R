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
#' \code{rvi_options} can include a vector of any additional commands to add to the rvi file prior to execution. Any
#' commands are added temporarily, and the rvi file is restored following the Raven run. The original rvi file
#' is backed up as a copy in case of an interruption of the function or other error to prevent loss of data.
#' The rvi commands provided are checked against a list of known rvi commands and a warning is issued
#' if an unrecognized command is provided, but all commands provided are nonetheless written to the temporary
#' rvi file as provided. All rvi commands should include the colon prefix to the command (e.g. ":SilentMode" not "SilentMode"),
#'  as this is not added automatically.
#'
#' Note that this function may not work in all servers, as some more specific setups when invoking the \code{system}
#' command may be required. In addition, errors may occur if the Raven.exe file does not have permission to execute. This
#' can be rectified with the \code{run_chmod} parameter set to \code{TRUE}
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
#' @param rvi_options string vector of additional options to add to rvi file temporarily for run
#' @param run_chmod runs a chmod system call to the provided executable ('chmod +x') (default \code{FALSE})
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
#' ## check that Raven.exe is downloaded
#' if (!rvn_download(check=TRUE)) {rvn_download()}
#'
#' # Irondequoit example
#' rvn_run(indir=paste(destdir,"/Irond",sep=""),
#'         showoutput=TRUE)
#'
#' # run in Silent Mode (rvi option)
#' rvn_run(indir=paste(destdir,"/Irond",sep=""),
#'         showoutput=TRUE,
#'         rvi_options=c(":SilentMode"))
#' }
#'
#' @export rvn_run
rvn_run <- function(fileprefix=NULL, indir=getwd(), ravenexe=NULL,
                  outdir=NULL,
                  rvc=NULL, rvt=NULL, rvp=NULL, rvh=NULL,
                  showoutput=FALSE, rvi_options=NULL,
                  run_chmod=FALSE) {

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

   if (run_chmod) {
      # run chmod to add execution permissions to ravenexe
      system(paste("chmod +x",ravenexe))
   }

   if (!dir.exists(indir)) {
      stop(sprintf("indir path of %s does not exist",indir))
   }

   if (!file.exists(file.path(indir,paste0(fileprefix,".rvi")))) {
      stop(sprintf("rvi file not found: %s",file.path(indir,paste0(fileprefix,".rvi")) ))
   }

   # # shell/system functions are vulnerable to paths with many characters or spaces
   # # here these paths are shortened: inputdir/ravenexe
   # if(Sys.info()["sysname"]=="Windows")
   # {
   #   ravenexe <- shortPathName(ravenexe)
   #   indir <- shortPathName(indir)
   # }

   # note: need to use different cross-platform function for sanitizing file paths
   # seems to work with spaces in paths as is, likely handled in system call

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

   ## check rvi options and store rvi / update rvi if valid options exist
   if (!is.null(rvi_options)) {

      # check options against known ones
      known_rvi_options <- get_rvi_options()

      if (any(rvi_options %notin% known_rvi_options)) {
         warning(sprintf("rvn_run: Some provided rvi options not recognized, and may result in warnings/errors in the Raven call:\n%s",
                         paste0(rvi_options[which(rvi_options %notin% known_rvi_options)],collapse="\n")  ))
      }

      # read in and backup rvi file
      rvi <- readLines(file.path(indir,paste0(fileprefix,".rvi")))
      backup_rvi_path <- paste0(file.path(indir,paste0(fileprefix,".rvi")),"__backup.rvi")
      writeLines(rvi, con=backup_rvi_path)

      # write options to rvi file
      writeLines(text=append(rvi, values=c(rvi_options, "")), con=file.path(indir,paste0(fileprefix,".rvi")))
   }

   if(Sys.info()["sysname"]=="Windows") {
     res <- invisible(system(RavenCMD, show.output.on.console = showoutput))
   } else {
     res <- system(RavenCMD)
   }

   # clean up rvi file if rvi_options used
   if (!is.null(rvi_options)) {

      # write back backup rvi
      writeLines(text=rvi, con=file.path(indir,paste0(fileprefix,".rvi")))

      if (file.exists(backup_rvi_path)) {
         unlink(backup_rvi_path)
      } else {
         warning(sprintf("rvn_run: rvi_options used and backup rvi file created, but cannot be located.\nPlease manually inspect your rvi file for new commands at the end of file:\n%s",
                         file.path(indir,paste0(fileprefix,".rvi"))  ))
      }
   }

   return(res)
}
