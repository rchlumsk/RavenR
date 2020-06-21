#' EC Climate Station File Conversion
#'
#' Note - this function is designated to use data from the weathercan package
#' available on Github to pull Environment Canada data
#'
#' rvn_ECmet_rvt converts Environment Canada historical meteorological data for a
#' given station into the .rvt format usable in Raven.
#'
#' The function prints in the :MultiData format; the particular set of forcings to
#' print can be set with the forcing.set command. The data should be downloaded
#' with missing days included. The download website is linked below.
#'
#' prd is used by the xts formatted-data to restrict the data reported in .rvt
#' files, for each station, to this period. The prd should be defined in
#' "YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
#' empty time series (i.e. non-overlapping time periods), an error will be
#' thrown.
#'
#' stnName can be supplied to overwrite the station name that is otherwise
#' obtained from the Station Name field in the climate file. Spaces in raw Station
#' Names will be replaced with underscores.
#'
#' prefix can be used to add a prefix to the .rvt file name, ("met_" by default)
#' which may be useful in organizing multiple climate data files.
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
#' (required for running in Raven, thus pre-processing of files is likely
#' required) - only handles daily data (subdaily or monthly not yet handled)
#'
#' @param metdata EC meteorological data from one or more stations (e.g., from weathercan::weather_dl())
#' @param prd (optional) data period to use in .rvt file
#' @param stnName (optional) station name to use (instead of name in file)
#' @param forcing.set (optional) specifies the set of forcings to print to file
#' @param prefix (optional) prefixes the file name (default: "met_")
#' @param write.redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param write.stndata (optional) write the gauge data to a separate .rvt file
#' @param rd.file (optional) name of the redirect file created (if write.redirect = TRUE)
#' @param stndata.file (optional) name of the station data file created (if write.stndata = TRUE)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @seealso \code{\link{rvn_ECflow_rvt}} to convert WSC flow gauge data to Raven format
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
#' \dontrun{
#' #-- Download data using weathercan weather_dl
#` kam <- weather_dl(station_ids = 51423,
#`                   start = "2016-10-01", end = "2019-09-30", interval="day")
#'
#' # basic use, includes "met_" prefix
#' # default forcing.set (PRECIP, MAX TEMP, MIN TEMP)
#' rvn_ECmet_rvt(metdata = kam, forcing.set = 1)
#'
#' # set without prefix, station data and redirect files created
#' # forcing.set 2 includes (RAINFALL, SNOWFALL, MAX TEMP, MIN TEMP)
#' rvn_ECmet_rvt(metdata = kam, forcing.set = 2, prefix = NULL, write.stndata = T, write.redirect = T)
#'
#' }
#'
#' @export rvn_ECmet_rvt
#'
rvn_ECmet_rvt <-  function (metdata, prd = NULL, stnName = NULL, forcing.set = 1, prefix = 'met_',
                         write.redirect = F, write.stndata = F, rd.file = "met_redirects.rvt",
                         stndata.file = "met_stndata.rvt") {

  ## params
  params <- c("max_temp","min_temp","mean_temp","total_rain","total_snow","total_precip")

  ## verify inputs
  if (nrow(metdata) <= 0) {
    stop("Requires at least one line of data")
  }
  # Number of data sets
  if (length(unique(metdata$station_id))>1) {
    print(paste("Imported", as.character(length(unique(metdata$station_id))), "data sets"))
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

  # Make sure dates are dates
  metdata$date <- as.Date(metdata$date)
  if (is.na(max(metdata$date))) {
    stop('date column not formated properly (e.g. "%Y-%m-%d"')
  }

  # Replace spaces in Station Names with underscores
  metdata$station_name <- gsub(" ", "_", metdata$station_name)

  # write status of each rvt file. Geographic data is written for every successful station into the main rvt metadata.
  ws <- data.frame("station" = unique(metdata$station_id), "rvt.name" = NA ,"status" = NA)

  ## Begin writing rvt file for each station listed in entry
  for (i in 1:length(unique(metdata$station_id))){
    rr <- metdata[metdata$station_id == unique(metdata$station_id)[i],]
    sn <- unique(rr$station_name)
    sid <- unique(rr$station_id)

    # Subset period if necessary
    if (!(is.null(prd))) {
      rr <- filter(rr, rr$date >= as.Date(firstsplit[1]), rr$date <= as.Date(firstsplit[2]))
    }

    # Verify existing record overlaps with desired period
    if(nrow(rr) == 0 & !is.null(prd)){
      message(paste0("No data available for desired period at ",sn," (station ID: ",sid,")"))
    }

    if ("time" %in% colnames(rr)){
      stop(paste0("Function not available yet for non-daily time steps. Please re-download data at daily intervals.
                   (Station ID:", sid,")"))
    }

    # Assuming dates are properly formatted
    dd <- rr$date # as.Date(format(as.POSIXct(rr$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d"))
    timestep <- as.numeric(as.difftime(dd[2:length(dd)]-dd[1:(length(dd)-1)], units = "days"))

    if(sum(timestep != 1)>0) {
      # Can't it handle if dt > 1? Also, should we not have the user pass this?
      stop(paste0("Function not available yet for non-daily time steps. Please check time intervals. (Station ID: ",sid,")"))
    }

    # create time series for extracted parameters
    if(forcing.set == 1){
      rr.ts <-  xts(x=rr[,c("total_precip","max_temp","min_temp")], order.by = dd)
    } else if(forcing.set == 2) {
      # verify snow measurements exist
      if("total_snow" %in% colnames(metdata)){
      rr.ts <-  xts(x=rr[,c("total_rain","total_snow","max_temp","min_temp")], order.by = dd)
      rr.ts$total_snow <-  rr.ts$total_snow*10 # conversion from cm to mm (Raven convention)
      } else {
        stop(paste0("Station does not have snowfall observations on record.\nLook for 'total_snow' as a parameter in the inputs. (Station id: ", sid,")"))
      }
    } else {
      stop("'forcing.set' value can only be set as 1 or 2. Type '?rvn_ECmet_rvt' for more details.")
    }

    #rr.ts[is.na(rr.ts)] = - 1.2345

    # write rvt file
    if(!is.null(stnName)){
      if(length(stnName != length(unique(metdata$station_id)))){
        stop("Number of assigned station names does not match number of input stations.")
      }else{
      sn <- stnName[i]
      }
    }

    rvt.name <- sprintf("%s.rvt",sn)
    if(!is.null(prefix)) {
      rvt.name <- sprintf("%s%s", prefix, rvt.name)
    }
    ws$rvt.name[ws$station==sid] <- rvt.name

    # -- Modified to use rvn_rvt_write
    #-- Data sets are subset & converted to dataframe prior to writing (rvn_rvt_write currently doens't handle tibbles)
    #   Dates are seperated out since we are not passing an XTS object

    if(forcing.set == 1){
      rvn_rvt_write(ts = rr.ts,
                    ff = rvt.name,
                    params = ":Parameters\tPRECIP\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN",
                    units  = ":Units\tmm/d\tC\tC")
    } else {
      rvn_rvt_write(ts = rr.ts,
                    ff = rvt.name,
                    params = ":Parameters\tRAINFALL\tSNOWFALL\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN",
                    units  = ":Units\tmm/d\tmm/d\tC\tC")
    }

    # Report success
    message(sprintf("Done writing to %s", rvt.name))
    ws$status[ws$station == sid] = TRUE

    # Provide a visual of generated time series
    print("Overview of generated timeseries")
    print(autoplot(rr.ts, geom="line", main = sn)+theme_bw()+theme(axis.title.x = element_blank()))
  }

  ## Write geographic values of stations to main rvt file
  md <-  do.call("rbind",lapply(ws$station[ws$status == T],function(s){
    md <- metdata[metdata$station_id==s, c("station_name","lat","lon","elev")]
    md <- data.frame(md[!duplicated(md),], "rvt.name" = ws$rvt.name[ws$station==s])
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
