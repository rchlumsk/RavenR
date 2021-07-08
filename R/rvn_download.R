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
#' Reinstalling the RavenR package removes the previous files in the RavenR/ folder on your system, so this command
#' will need to be re-run if RavenR is re-installed.
#'
#' Once downloaded, the Raven.exe file can be found with \code{system.file("extdata", "Raven.exe", package="RavenR")}
#'
#' @param version (optional) Character: The version of Raven to be downloaded. If not provided, the latest version will be downloaded.
#' @param netCDF boolean whether to download the NetCDF-enabled version of Raven (default \code{FALSE})
#' @return Returns \code{TRUE} if executed successfully
#'
#' @seealso \code{\link{rvn_run}}
#'
#' @examples
#' \dontrun{
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
#' @importFrom RCurl url.exists
#' @importFrom utils win.version unzip download.file
rvn_download<-function(version=NA,NetCDF=FALSE)
{
   # path<-paste(.libPaths()[1],"/RavenR/",sep="")
   # path1<-paste(.libPaths()[1],"/RavenR",sep="")

   download_path <- tempdir()
   save_path <- system.file("extdata",package="RavenR")

   # path1 <- system.file("extdata",package="RavenR")
   # path <- sprintf("%s/",path1)

   # rvn_get_os <- function()
   # {
   #    sysinf <- Sys.info()
   #    if (!is.null(sysinf))
   #    {
   #       os <- sysinf["sysname"]
   #       if (os == "Darwin") os <- "osx"
   #    }else{
   #       os <- .Platform$OS.type
   #       if (grepl("^darwin", R.version$os))   os <- "osx"
   #       if (grepl("linux-gnu", R.version$os)) os <- "linux"
   #    }
   #    tolower(os)
   # }
   # platform <- rvn_get_os()
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
      arch<-win.version()
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
   if(!url.exists(downloadLink)) stop("Bad file url. Check the repository!")

   # save to download_path (from tempdir()), then copy to RavenR/extdata folder
   download.file(downloadLink,paste(download_path,"/",fileName,sep=""))
   unzip(zipfile=paste(download_path,"/",fileName,sep=""),exdir=download_path)
   file.remove(paste(download_path,"/",fileName,sep=""))
   file.remove(paste(download_path,"/","RavenManual","_",gsub("n","",selectedVersion),".pdf",sep=""))
   res <- file.copy(from=paste(download_path,"/","Raven.exe",sep=""),
             to=paste(save_path,"/","Raven.exe",sep=""),
             overwrite=TRUE)

   # if(NetCDF)  file.rename(paste(path,"Raven.exe",sep=""),paste(path,"Raven_NetCDF.exe",sep=""))
   # if(!NetCDF) file.rename(paste(path,"Raven.exe",sep=""),paste(path,"Raven_.exe",sep=""))
   return(res)
}