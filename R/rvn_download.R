#' @title Downloads Raven
#'
#' @description
#' Downloads Raven executable from the \href{http://raven.uwaterloo.ca/Downloads.html}{Raven webpage}.
#'
#' @details
#' Files are downloaded from the Raven webpage (\url{http://raven.uwaterloo.ca/Downloads.html}) and placed
#' in a temporary directory while the executable is extracted. This helps prevent any unwanted files being
#' saved on your system outside of temporary directories if the function is interrupted. The executable is placed in the
#' RavenR/extdata directory, wherever RavenR is installed on your system.
#'
#' Note that if you are not on a Windows operating system, you may need to compile Raven for your system rather than
#'
#' Any existing Raven.exe file in your RavenR/extdata folder will be overwritten with a newly downloaded executable w
#' with this function.
#'
#' The `copy_path` argument may be useful for non-Windows users that are not able to use the pre-compiled Raven downloads.
#' If the path to an existing Raven.exe file (i.e. one compiled locally) is provided, this will copy the specified file to the
#' appropriate location and skip the download, which will allow the user to use the locally compiled Raven.exe with the `rvn_run`
#' function.
#'
#' Reinstalling the RavenR package removes the previous files in the RavenR/ folder on your system, so this command
#' will need to be re-run if RavenR is re-installed.
#'
#' Once downloaded, the Raven.exe file can be found with \code{system.file("extdata", "Raven.exe", package="RavenR")}
#'
#' @param version (optional) Character: The version of Raven to be downloaded. If not provided, the latest version will be downloaded.
#' @param NetCDF (logical) whether to download the NetCDF-enabled version of Raven (default \code{FALSE})
#' @param check (logical) if \code{TRUE}, function will only check whether 'Raven.exe' has been downloaded to the RavenR folder
#' @param copy_path (character) path to an existing 'Raven.exe' file. If provided, this file will be copied to the appropriate folder and the download will be skipped.
#' @return Returns \code{TRUE} if executed successfully
#'
#' @seealso \code{\link{rvn_run}}
#'
#' @examples
#'
#' # check if Raven.exe has previously been downloaded
#' rvn_download(check=TRUE)
#'
#' \dontrun{
#' ## NOT RUN (downloads executable)
#'
#' # download latest without netcdf support
#' rvn_download()
#'
#' # download specific version with netcdf support
#' rvn_download(version="3.0.4",NetCDF=TRUE)
#'
#' # find file path to Raven.exe
#' system.file("extdata", "Raven.exe", package="RavenR")
#' }
#'
#' @export rvn_download
#' @importFrom stringr str_match_all str_match
#' @importFrom utils osVersion unzip download.file
#' @importFrom RCurl url.exists
rvn_download<-function(version=NA,NetCDF=FALSE,check=FALSE,copy_path=NULL)
{
   . <- NULL

   download_path <- tempdir()
   save_path <- system.file("extdata",package="RavenR")

   if (check) {

      if (file.exists(sprintf("%s/Raven.exe",save_path))) {
         message(sprintf("Raven.exe found in %s", save_path))
         return(TRUE)
      } else {
         message(sprintf("Raven.exe NOT found in %s", save_path))
         return(FALSE)
      }
   }

   if (is.null(copy_path)) {
      sysinf <- Sys.info()
      if (!is.null(sysinf))
      {
         os <- sysinf["sysname"]
         if (os == "Darwin") os <- "osx"
      }else{
         os <- .Platform$OS.type
         if (grepl("^darwin", R.version$os))   os <- "osx"
         if (grepl("linux-gnu", R.version$os)) os <- "linux"
      }
      platform <-  tolower(os)

      url <- "http://raven.uwaterloo.ca/Downloads.html"
      html <- paste(readLines(url), collapse="\n")
      matched <- str_match_all(html, "<a href=\"(.*?)\"")[[1]]
      matched<-matched[grep("a href=",matched)]

      if(platform=="windows")
      {
         arch<-osVersion
         if (length(grep("x64",arch))==1)
         {
            versions<-matched[grep("Win64",matched)]
            versions<-str_match(versions, "Win64_\\s*(.*?)\\s*.zip")[,2]
            if(NetCDF)
            {
               versions<-versions[grep("n",versions)]
               if(length(versions)<1) stop(paste("A Raven version supporting NetCDF on: ",arch," is not available!"))
               versions<-gsub("[^0-9.-]", "", versions) %>%
                  sort(.,decreasing=TRUE) %>%
                  paste("v",.,"n",sep="")
               if(!is.na(version))
               {
                  selectedVersion<-versions[paste("v",version,"n",sep="")%>% match(.,versions)]
                  if(is.na(selectedVersion)) stop (paste("A NetCDF version",version, "is not available for",arch,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
               }else{
                  selectedVersion<-versions[1]
               }
            }else{
               if(length(grep('n',versions))>0) versions<-versions[-grep('n',versions)]
               versions<-gsub("[^0-9.-]", "", versions)
               if(length(versions)<1) stop(paste("A Raven version on: ",arch," is not available!"))
               versions<-versions[!duplicated(versions)]%>% sort(.,decreasing=TRUE)%>% paste("v",.,sep="")
               if(!is.na(version))
               {
                  selectedVersion<-versions[paste("v",version,sep="")%>% match(.,versions)]
                  if(is.na(selectedVersion)) stop (paste("Version",version, "is not available for",arch,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
               }else{
                  selectedVersion<-versions[1]
               }
            }
            fileName<-paste("RavenExecutableWin64_",selectedVersion,".zip",sep="")
         }
         if (length(grep("x86",arch))==1)
         {
            versions<-matched[grep("Win32",matched)]
            versions<-str_match(versions, "Win32_\\s*(.*?)\\s*.zip")[,2]
            if(NetCDF)
            {
               versions<-versions[grep("n",versions)]
               if(length(versions)<1) stop(paste("A Raven version supporting NetCDF on: ",arch," is not available!"))
               versions<-gsub("[^0-9.-]", "", versions) %>%
                  sort(.,decreasing=TRUE) %>%
                  paste("v",.,"n",sep="")
               if(!is.na(version))
               {
                  selectedVersion<-versions[paste("v",version,"n",sep="")%>% match(.,versions)]
                  if(is.na(selectedVersion)) stop (paste("A NetCDF version",version, "is not available for",arch,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
               }else{
                  selectedVersion<-versions[1]
               }
            }else{
               if(length(grep('n',versions))>0) versions<-versions[-grep('n',versions)]
               versions<-gsub("[^0-9.-]", "", versions)
               if(length(versions)<1) stop(paste("A Raven version on: ",arch," is not available!"))
               versions<-versions[!duplicated(versions)]%>% sort(.,decreasing=TRUE)%>% paste("v",.,sep="")
               if(!is.na(version))
               {
                  selectedVersion<-versions[paste("v",version,sep="")%>% match(.,versions)]
                  if(is.na(selectedVersion)) stop (paste("Version",version, "is not available for",arch,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
               }else{
                  selectedVersion<-versions[1]
               }
            }
            fileName<-paste("RavenExecutableWin32_",selectedVersion,".zip",sep="")
         }
      }
      if(platform=="osx")
      {
         versions<-matched[grep("MacOS",matched)]
         versions<-str_match(versions, "MacOS_\\s*(.*?)\\s*.zip")[,2]
         if(NetCDF)
         {
            versions<-versions[grep("n",versions)]
            if(length(versions)<1) stop(paste("A Raven version supporting NetCDF on: ",arch," is not available!"))
            versions<-gsub("[^0-9.-]", "", versions) %>% sort(.,decreasing=TRUE) %>% paste("v",.,"n",sep="")
            if(!is.na(version))
            {
               selectedVersion<-versions[paste("v",version,"n",sep="")%>% match(.,versions)]
               if(is.na(selectedVersion)) stop (paste("A NetCDF version",version, "is not available for",platform,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
            }else{
               selectedVersion<-versions[1]
            }
         }else{
            if(length(grep('n',versions))>0) versions<-versions[-grep('n',versions)]
            versions<-gsub("[^0-9.-]", "", versions)
            versions<-versions[!duplicated(versions)]%>% sort(.,decreasing=TRUE)%>% paste("v",.,sep="")
            if(!is.na(version))
            {
               selectedVersion<-versions[paste("v",version,sep="")%>% match(.,versions)]
               if(is.na(selectedVersion)) stop (paste("Version",version, "is not available for",platform,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
            }else{
               selectedVersion<-versions[1]
            }
         }
         fileName<-paste("RavenExecutableMacOS_",selectedVersion,".zip",sep="")
      }
      if(platform=="linux")
      {
         versions<-matched[grep("Linux",matched)]
         versions<-str_match(versions, "Linux_\\s*(.*?)\\s*.zip")[,2]
         if(NetCDF)
         {
            versions<-versions[grep("n",versions)]
            if(length(versions)<1) stop(paste("A Raven version supporting NetCDF on: ",platform," is not available!"))
            versions<-gsub("[^0-9.-]", "", versions) %>% sort(.,decreasing=TRUE) %>% paste("v",.,"n",sep="")
            if(!is.na(version))
            {
               selectedVersion<-versions[paste("v",version,"n",sep="")%>% match(.,versions)]
               if(is.na(selectedVersion)) stop (paste("A NetCDF version",version, "is not available for",platform,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
            }else{
               selectedVersion<-versions[1]
            }
         }else{
            if(length(grep('n',versions))>0) versions<-versions[-grep('n',versions)]
            versions<-gsub("[^0-9.-]", "", versions)
            versions<-versions[!duplicated(versions)]%>% sort(.,decreasing=TRUE)%>% paste("v",.,sep="")
            if(!is.na(version))
            {
               selectedVersion<-versions[paste("v",version,sep="")%>% match(.,versions)]
               if(is.na(selectedVersion)) stop (paste("Version",version, "is not available for",platform,". Try either of [",paste(gsub("[^0-9.-]", "", versions),collapse=' , '),"] version(s)"))
            }else{
               selectedVersion<-versions[1]
            }
         }
         fileName<-paste("RavenExecutableLinux_",selectedVersion,".zip",sep="")
      }
      downloadLink<-paste("http://raven.uwaterloo.ca/files/",gsub("n","",selectedVersion),"/",fileName,sep="")
      if(!RCurl::url.exists(downloadLink)) stop("Bad file url. Check the repository!")

      # save to download_path (from tempdir()), then copy to RavenR/extdata folder
      download.file(downloadLink,paste(download_path,"/",fileName,sep=""))
      unzip(zipfile=paste(download_path,"/",fileName,sep=""),exdir=download_path)
      file.remove(paste(download_path,"/",fileName,sep=""))
      file.remove(paste(download_path,"/","RavenManual","_",gsub("n","",selectedVersion),".pdf",sep=""))
      res <- file.copy(from=paste(download_path,"/","Raven.exe",sep=""),
                       to=paste(save_path,"/","Raven.exe",sep=""),
                       overwrite=TRUE)
   } else {

      ## copy local provided file path, do not download Raven.exe

      if (!file.exists(copy_path) ) {
         stop("copy_path is not a valid file, please check")
      } else {

         if (rvn_substrRight(copy_path,9) != "Raven.exe") {
            warning("expecting a valid Raven.exe file, file will be renamed to Raven.exe when copied to local RavenR folder.")
         }

         res <- file.copy(from=copy_path,
                   to=paste(save_path,"/","Raven.exe",sep=""),
                   overwrite-TRUE)
      }
   }
   return(res)
}
