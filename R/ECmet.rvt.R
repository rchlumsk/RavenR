#' EC Climate Station File Conversion
#'
#' Note - this function is designated to be rewritten using the weathercan package
#' available on Github to pull Environment Canada data directly
#'
#' ECmet.rvt converts Environment Canada historical meteorological data for a
#' given station into the .rvt format usable in Raven.
#'
#' This function accepts a set of files and parses them into a single time
#' series in the .rvt format for Raven. The function prints in the :MultiData
#' format; the particular set of forcings to print can be set with the
#' forcing.set command. The files should be downloaded in a .csv Date-Data
#' format with missing days included. The download website is linked below.
#' Note that the first file passed is used to extract the gauge data, and the
#' remaining files are compared to the ClimateID of the first file to ensure
#' consistency.
#'
#' ff should be a string vector (or single string) of climate file paths. The
#' first file is used to obtain the station metadata, and the subsequent files
#' are checked for consistency against the first file to ensure there is not a
#' mix of climate stations supplied. The files do not have to be supplied in
#' chronological order as long as they belong to the same climate station.
#'
#' prd is used by the xts formatted-data to restrict the data reported in .rvt
#' files, for each station, to this period. The prd should be defined in
#' "YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
#' empty time series (i.e. non-overlapping time periods), an error will be
#' thrown.
#'
#' stnName can be supplied to overwrite the station name that is otherwise
#' obtained from the Station Name field in the climate file.
#'
#' met.prefix supplies the "met_" prefix to the .rvt file name, which may be
#' useful in organizing multiple climate data files.
#'
#' forcing.set specifies the set of forcings to print to .rvt file. Currently
#' there are only two available sets. A value of 1 prints total precipitation,
#' and 2 splits the precipitation into rainfall and snowfall. In some cases the
#' EC data provides only total precipitation, which is a good check to make for
#' the particular climate station before printing rvt files. Both sets
#' currently print max and min daily temperature. Future extensions to this
#' function may provide more options for forcing sets.
#'
#' write.redirect will print out the :RedirectToFile commands in a separate
#' file, met_redirects.rvt. These commands can be copied into the main model's
#' .rvt file to redirect to the produced time series files. The function will
#' append to the file if it already exists, meaning that this works for
#' iterations of this function.
#'
#' write.stndata wil print out the gauge metadata to file (met_stndata.rvt) in
#' the .rvt format, which is required to include a meterological station in
#' Raven. The function will append to the file if it already exists, meaning
#' that this works for iterations of this function.
#'
#' perform.qc is currently under construction and is not yet available; setting
#' to TRUE will result in an warning.
#'
#' The function has several built-in data quality checks. These include: -
#' checking that all supplied files are for the same climate station - ensuring
#' the timestep (data resolution) is the same in each file - automatically
#' combining time series and ensuring there are no gaps in the data supplied
#' (i.e. time gaps, not missing values) - check for missing data and issuing a
#' warning that post-processing will be required
#'
#' ### FUNCTION IS CURRENTLY IN BETA MODE AND UNDERGOING TESTING + UPDATES ###
#'
#' Current limitations of the function: - quality control is not implemented;
#' does not check for common errors in data, does not infill missing values
#' (required for running in Raven, thus post-processing of files is likely
#' required) - only handles daily data (subdaily or monthly not yet handled)
#'
#' @param ff EC climate data file paths in a string vector
#' @param prd (optional) data period to use in .rvt file
#' @param stnName (optional) station name to use (instead of name in file)
#' @param forcing.set (optional) specifies the set of forcings to print to file
#' @param met.prefix (optional) prefixes the file name with "met_"
#' @param write.redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param write.stndata (optional) write the gauge data to a separate .rvt file
#' @param perform.qc (optional) quality controls the data before writing to
#' file (not yet implemented!)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{ECflow.rvt}} to convert WSC flow gauge data to Raven
#' format
#'
#' Download EC climate data from
#' \href{http://climate.weather.gc.ca/historical_data/search_historic_data_e.html}{EC
#' Historical Data}
#'
#' Download multiple years of climate data, see
#' \href{ftp://ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Readme.txt}{instructions}
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#' @keywords Raven meteorological station rvt conversion
#' @examples
#' # warning: example not run, sample example for associated files only
#' \dontrun{
#' ff <- c("eng-daily-01012008-12312008.csv","eng-daily-01012007-12312007.csv")
#'
#' # basic use, includes "met_" prefix
#' # default forcing.set (PRECIP, MAX TEMP, MIN TEMP)
#' ECmet.rvt(ff,forcing.set=1)
#'
#' # set without prefix, different file created
#'   # forcing.set includes (RAINFALL, SNOWFALL, MAX TEMP, MIN TEMP)
#' ECmet.rvt(ff,forcing.set=2,met.prefix=F,write.stndata=T,write.redirect = T)
#' }
#'
#' @export ECmet.rvt
ECmet.rvt <- function(ff,prd=NULL,stnName=NULL,forcing.set=1,met.prefix=T,write.redirect=F,write.stndata=F,perform.qc=F) {

  # eventually want to update to handle multiple files from different stations,
  #   and spit everything out nicely
  # for now just assume all input files are for the same station, and ignore those that are not
  # handling different stations will require creating a vector of R objects, a bit more complicated
  ##################################
  # all.climateIDs = NULL
  # rvt.obj <- setClass('metobj',slots=c(stnName='character',climateID='integer',dd='POSIXct'))
  # rvt.object.list <- list()

  # forcing.set
  # 1 = precip, daily max temp, daily min temp
  # 2 = rain, snow, daily max temp, daily min temp

  # hardcoded file names for optional printouts
  rd.file <- 'met_redirects.rvt'
  stndata.file <- 'met_stndata.rvt'

  # assumed hardcoded column indices for the rr.ts object (see below)
  max.temp.ind <- 1
  min.temp.ind <- 2
  mean.temp.ind <- 3
  tot.rain.ind <- 6
  tot.snow.ind <- 7
  tot.precip.ind <- 8

  if (perform.qc) {
    warning("Sorry, quality control not available yet.")
  }

  # check number of files to read in
  if (length(ff) == 0 ) {
    stop("Requires at least one file to read in.")
  }

  # check format of the prd argument
  if (!(is.null(prd))) {

    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
  }

  # read in file first, locate data row indices
  rr <- readLines(ff[1],n=50)
  sname.ind <- grep("Station Name",rr,fixed=TRUE)
  lat.ind <- grep("Latitude",rr,fixed=TRUE)
  long.ind <- grep("Longitude",rr,fixed=TRUE)
  elev.ind <- grep("Elevation",rr,fixed=TRUE)
  climateID.ind <- grep("Climate Identifier",rr,fixed=TRUE)
  dt.ind <- grep("Date/Time",rr,fixed=TRUE)

  # re-read as csv file, parse metadata
  rr <- readLines(ff[1],n=dt.ind-1)
    # find rr by index, take out extra characters and replace spaces with underscores
  sname <- gsub(" ","_",gsub("\"","",unlist(strsplit(rr[sname.ind],","))[2]))
    # consider setting the sname to lower case, although can be overwritten by the stnName argument
  climateID <- as.numeric(gsub("\"","",unlist(strsplit(rr[climateID.ind],","))[2]))
  lat <- as.numeric(gsub("\"","",unlist(strsplit(rr[lat.ind],","))[2]))
  long <- as.numeric(gsub("\"","",unlist(strsplit(rr[long.ind],","))[2]))
  elev <- as.numeric(gsub("\"","",unlist(strsplit(rr[elev.ind],","))[2]))

  # parse data in file
  rr <- utils::read.table(ff[1],header=T,sep = ",", quote = "\"",  dec = ".",skip=dt.ind-1,stringsAsFactors=F)
  find.time=F
  if (any(grepl("Time",colnames(rr)[2:5]))) {
    dd <- as.POSIXct(paste(rr$Year,rr$Month,rr$Day,rr$Time,sep="-"),format="%Y-%m-%d %H:%M:%S")
    find.time=T
  } else {
    dd <- as.POSIXct(paste(rr$Year,rr$Month,rr$Day,sep="-"),format="%Y-%m-%d")
  }
  if (find.time) {
    rr <- rr[,-c(1:6)]
  } else {
    rr <- rr[,-c(1:5)]
  }

  # create time series - very dependent on format, check this here
  rr.ts <- xts(x=rr[,seq(1,19,by=2)],order.by=dd)

  # determine timestep in data
  timestep <- as.numeric(as.difftime(as.Date(dd[2])-as.Date(dd[1]),units='days'))

  if (timestep != 1) {
    stop("Function not available yet for non-daily timesteps.")
  }

  # replace missing data / qaqc
    # to be filled

  # iterate for other files in ff -----
  if (length(ff) > 1) {

    for (i in 2:length(ff)) {
      # read in file first, locate data row indices
      rr <- readLines(ff[i],n=50)
      climateID.ind <- grep("Climate Identifier",rr,fixed=TRUE)
      dt.ind <- grep("Date/Time",rr,fixed=TRUE)

      # re-read as csv file, parse metadata
      rr <- readLines(ff[i],n=dt.ind-1)
      # find rr by index, take out extra characters and replace spaces with underscores
      climateID.temp <- as.numeric(gsub("\"","",unlist(strsplit(rr[climateID.ind],","))[2]))

      if (climateID.temp == climateID) {

        # parse data in file
        rr <- utils::read.table(ff[i],header=T,sep = ",", quote = "\"",  dec = ".",skip=dt.ind-1,stringsAsFactors=F)
        find.time=F
        if (any(grepl("Time",colnames(rr)[2:5]))) {
          dd <- as.POSIXct(paste(rr$Year,rr$Month,rr$Day,rr$Time,sep="-"),format="%Y-%m-%d %H:%M:%S")
          find.time=T
        } else {
          dd <- as.POSIXct(paste(rr$Year,rr$Month,rr$Day,sep="-"),format="%Y-%m-%d")
        }
        if (find.time) {
          rr <- rr[,-c(1:6)]
        } else {
          rr <- rr[,-c(1:5)]
        }

        # create time series - very dependent on format, check this here
        rr.ts.temp <- xts(x=rr[,seq(1,19,by=2)],order.by=dd)

        # determine timestep in data
        timestep.temp <- as.numeric(as.difftime(as.Date(dd[2])-as.Date(dd[1]),units='days'))

        if (timestep == timestep.temp) {
          # merge time series together
          rr.ts <- rbind(rr.ts, rr.ts.temp)

        } else {
          warning(sprintf("Timestep is different from base file in %s, file ignored.",ff[i]))
        }

      } else {
        warning(sprintf("Diffferent Climate identifier from first file in %s, file ignored",ff[i]))
      }

    }
  }

  # additional xts manipulations -----

    # check if the time series is missing any dates
  if (length(seq.Date(from=lubridate::date(rr.ts[1]),to=lubridate::date(rr.ts[nrow(rr.ts)]),by=1)) != nrow(rr.ts)) {
    stop("Gap exists in the dates of the supplied files; please supply a continuous set of met data. Rvt files not written.")
  }

  # generate prd if not supplied
  if (is.null(prd)) {
    N <- nrow(rr.ts)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",year(rr.ts[1,1]),month(rr.ts[1,1]),day(rr.ts[1,1]),
                   year(rr.ts[N,1]),month(rr.ts[N,1]),day(rr.ts[N,1]) )
  }

  # adjust rr.ts to the supplied prd
  rr.ts <- rr.ts[prd,]

  # quality control -- NEED TO UPDATE THIS SECTION WITH MORE QC
    # replace all NA values with Raven missing value code
  rr.ts[is.na(rr.ts)] = -1.2345

  # write rvt files -----
  if (is.null(stnName)) {
    stnName <- sname
  }

  rvt.name <- sprintf("%s.rvt",stnName)
  if (met.prefix) {
    rvt.name <- sprintf('met_%s',rvt.name)
  }

  if (write.redirect) {
    fc.redirect <- file(rd.file,open='a+')
    writeLines(sprintf(':RedirectToFile %s',rvt.name),fc.redirect)
    close(fc.redirect)
  }

  if (write.stndata) {
    fc <- file(stndata.file,open='a+')
    writeLines(sprintf(':Gauge %s',stnName),fc)
    writeLines(sprintf('  :Latitude %.6f',lat),fc)
    writeLines(sprintf('  :Longitude %.6f',long),fc)
    writeLines(sprintf('  :Elevation %.2f',elev),fc)
    writeLines(sprintf('  :RedirectToFile %s',rvt.name),fc)
    writeLines(":EndGauge\n",fc)
    close(fc)
  }

  if (forcing.set == 1) {
    if (any(c(rr.ts[,tot.precip.ind],rr.ts[,max.temp.ind],rr.ts[,min.temp.ind])==-1.2345)) {
      warning("Missing values found in time series, post-processing will be required for use in Raven. Missing values written to file as -1.2345.")
    }
  } else if (forcing.set == 2) {
    if (any(c(rr.ts[,tot.rain.ind],rr.ts[,tot.snow.ind],rr.ts[,max.temp.ind],rr.ts[,min.temp.ind])==-1.2345)) {
      warning("Missing values found in time series, post-processing will be required for use in Raven. Missing values written to file as -1.2345.")
    }
  }

  # write main rvt file
  fc <- file(rvt.name,open='w+')
  writeLines(':MultiData',fc)
  writeLines(sprintf('%s 00:00:00 %g %i',as.character(lubridate::date(rr.ts[1])),timestep,nrow(rr.ts)),fc)
    # eventually update this to be based on a selection of custom-defined columns
  if (forcing.set == 1) {
    writeLines(':Parameters\tPRECIP\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN',fc)
    writeLines(':Units\tmm/d\tC\tC',fc)
    for (j in 1:nrow(rr.ts)) {
      writeLines(sprintf('\t%g\t%g\t%g',rr.ts[j,tot.precip.ind],rr.ts[j,max.temp.ind],rr.ts[j,min.temp.ind]),fc)
    }
  } else if (forcing.set == 2) {
    writeLines(':Parameters\tRAINFALL\tSNOWFALL\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN',fc)
    writeLines(':Units\tmm/d\tmm/d\tC\tC',fc)
    for (j in 1:nrow(rr.ts)) {
      writeLines(sprintf('\t%g\t%g\t%g\t%g',rr.ts[j,tot.rain.ind],rr.ts[j,tot.snow.ind],rr.ts[j,max.temp.ind],rr.ts[j,min.temp.ind]),fc)
    }
  } else {
    stop(sprintf("forcing.set %i not a defined option.",forcing.set))
  }
  writeLines(':EndMultiData',fc)
  close(fc)
  print(sprintf("Done writing to %s",rvt.name))

  return(TRUE)
}

