#' EC Climate Station File Conversion
#'
#' Note - this function is designated to be rewritten using the weathercan package
#' available on Github to pull Environment Canada data directly
#'
#' rvn_ECmet_rvt converts Environment Canada historical meteorological data for a
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
#' @seealso \code{\link{rvn_ECflow_rvt}} to convert WSC flow gauge data to Raven
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
#' rvn_ECmet_rvt(ff,forcing.set=1)
#'
#' # set without prefix, different file created
#'   # forcing.set includes (RAINFALL, SNOWFALL, MAX TEMP, MIN TEMP)
#' rvn_ECmet_rvt(ff,forcing.set=2,met.prefix=F,write.stndata=T,write.redirect = T)
#'
#'
#' # sample data set from weatherCAN
#' ff = weather_dl(station_ids=c(4180,4181), interval = "day")
#'
#' library dependencies:
#' library(weathercan)
#' library(xts)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' }
#'
#' @export rvn_ECmet_rvt
rvn_ECmet_rvt = function (ff, prd = NULL, stnName = NULL, forcing.set = 1, met.prefix = T,
                         write.redirect = F, write.stndata = F) {
  ## Define output file names
  rd.file <- "met_redirects.rvt"
  stndata.file <- "met_stndata.rvt"
  params = c("max_temp","min_temp","mean_temp","total_rain","total_snow","total_precip")

  ## verify inputs
  if (nrow(ff) == 0) {
    stop("Requires at least one set of station data to read in.")
  }
  # Number of data sets
  if (length(unique(ff$station_id))>1) {
    print(paste("Imported", as.character(length(unique(ff$station_id))), "data sets"))
  } else {
    print("Imported 1 data set")
  }
  # Confining dates for desired period
  if (!(is.null(prd))) {
    firstsplit <- unlist(strsplit(prd, "/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1], "-"))) != 3 ||
        length(unlist(strsplit(firstsplit[2], "-"))) != 3 ||
        nchar(firstsplit[1]) != 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
  }

  # write status of each rvt file. Geographic data is written for every successful station into the main rvt metadata.
  ws = data.frame("station" = unique(ff$station_id), "rvt.name" = NA ,"status" = NA)

  ## Begin writing rvt file for each station listed in entry
  for (i in 1:length(unique(ff$station_id))){
    rr = ff[ff$station_id == unique(ff$station_id)[i],]
    sn = unique(rr$station_name)
    sid = unique(rr$station_id)
    rr = rr[as.Date(rr$date) >= as.Date(firstsplit[1]) | as.Date(rr$date) <= as.Date(firstsplit[2]),]

    # Verify existing record overlaps with desired period
    if(nrow(rr) == 0 & !is.null(prd)){
      message(paste0("No data available for desired period at ",sn," (station ID: ",sid,")"))
    }

    if ("time" %in% colnames(rr)){
      stop(paste0("Function not available yet for non-daily time steps. Please re-download data at daily intervals.
                   (Station ID:", sid,")"))
    }

    dd = as.Date(format(as.POSIXct(rr$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d"))
    timestep = as.numeric(as.difftime(as.Date(dd[2:length(dd)])-as.Date(dd[1:(length(dd)-1)]), units = "days"))

    if(sum(timestep != 1)>0) {
      stop(paste0("Function not available yet for non-daily time steps. Please check time intervals. (Station ID: ",sid,")"))
    }

    # create time series for extracted parameters
    if(forcing.set == 1){
      rr.ts = xts(x=rr[,c("total_precip","max_temp","min_temp")], order.by = dd)
    }else if(forcing.set == 2){
      # verify snow measurements exist
      if("total_snow" %in% colnames(ff)){
      rr.ts = xts(x=rr[,c("total_rain","total_snow","max_temp","min_temp")], order.by = dd)
      rr.ts$total_snow = rr.ts$total_snow*10 # conversion from cm to mm (Raven convention)
      }else{
        stop(paste0("Station does not have snowfall observations on record.\nLook for 'total_snow' as a parameter in the inputs. (Station id: ", sid,")"))
      }
    }else{
      stop("'forcing.set' value can only be set as 1 or 2. Type '?rvn_ECmet_rvt' for more details.")
    }

    rr.ts[is.na(rr.ts)] = - 1.2345

    # write rvt file
    if(!is.null(stnName)){
      if(length(stnName != length(unique(ff$station_id)))){
        stop("Number of assigned station names does not match number of input stations.")
      }else{
      sn <- stnName[i]
      }
    }

    rvt.name = sprintf("%s.rvt",sn)
    if(met.prefix){
      rvt.name = sprintf("met_%s",rvt.name)
    }
    ws$rvt.name[ws$station==sid] = rvt.name

    fc <- file(rvt.name, open="w+")
    writeLines(":MultiData",fc)
    writeLines(sprintf("%s 00:00:00 %g %i", as.character(lubridate::date(rr.ts[1])), timestep[!duplicated(timestep)], nrow(rr.ts)),fc)

      if(forcing.set==1){
        writeLines(":Parameters\tPRECIP\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN",fc)
        writeLines(":Units\tmm/d\tC\tC",fc)
        for (j in 1:nrow(rr.ts)){
        writeLines(sprintf('\t%g\t%g\t%g', rr.ts$total_precip[j], rr.ts$max_temp[j], rr.ts$min_temp[j]),fc)
        }
      }else{
        writeLines(":Parameters\tRAINFALL\tSNOWFALL\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN",fc)
        writeLines(":Units\tmm/d\tmm/d\tC\tC",fc)
        for (j in 1:nrow(rr.ts)){
        writeLines(sprintf('\t%g\t%g\t%g\t%g', rr.ts$total_rain[j], rr.ts$total_snow[j], rr.ts$max_temp[j], rr.ts$min_temp[j]),fc)
        }
      }

    writeLines(':EndMultiData',fc)
    close(fc)
    message(sprintf("Done writing to %s", rvt.name))
    ws$status[ws$station == sid] = TRUE

    # Provide a visual of generated time series
    print("Overview of generated timeseries")
    print(autoplot(rr.ts, geom="line", main = sn)+theme_bw()+theme(axis.title.x = element_blank()))
  }

  ## Write geographic values of stations to main rvt file
  md = do.call("rbind",lapply(ws$station[ws$status == T],function(s){
    md = ff[ff$station_id==s, c("station_name","lat","lon","elev")]
    md = data.frame(md[!duplicated(md),], "rvt.name" = ws$rvt.name[ws$station==s])
  }))

  # Write station data
  if(write.stndata){
  fc = file(stndata.file,open = "a+")
  for (k in 1:nrow(md)){
    writeLines(sprintf(":Gauge %s", md$station_name[k]),fc)
    writeLines(sprintf("  :Latitude %.6f", md$lat[k]),fc)
    writeLines(sprintf("  :Longtiude: %.6f", md$lon[k]),fc)
    writeLines(sprintf("  :Elevation %.2f", md$elev[k]),fc)
    writeLines(":EndGauge\n",fc)
  }
  close(fc)
  message(sprintf("Done writing station data to %s", stndata.file))
  }

  ## Write Redirect commands
  if(write.redirect){
  fc.redirect = file(rd.file, open = "a+")
   for(k in 1:nrow(md)){
     writeLines(sprintf(":RedirectToFile %s", md$rvt.name[k]),fc.redirect)
   }
  close(fc.redirect)
  message(sprintf("Done writing redirect commands to %s",rd.file))
  }

  return(TRUE)
}
