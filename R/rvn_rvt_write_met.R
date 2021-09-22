#' @title EC Climate Station File Conversion
#'
#' @description
#' Converts meteorological data for a given station into the
#' Raven .rvt format.
#'
#' @details
#' Writes data in either :Data or :MultiData format depending on the number of
#' supported parameter columns provided. The data should be downloaded and prepared
#' with missing days included, and preferably is downloaded directly using \code{weathercan}.
#'
#' metdata contains all of the meteorological data to be written to file. metdata should be
#' in a tibble or data frame format, and is required to have a date/time column (either DATE,
#' TIME, or DATETIME, all of which have at least a Date component), the STATION_NAME,
#' and all desired forcings to be written to file. If the columns ELEV, LAT, LON are included,
#' the station meta data may be written to a separate rvt file as well (see below). All supported
#' data columns in metdata are written into rvt format, so the desired columns
#' for the rvt file should be passed through metdata and filtered first if needed.
#'
#' rvt_met_mapping is a list that maps the metdata column names to Raven variables. If
#' \code{weathercan} is used then this may be left NULL, as the mapping is automatically provided.
#' Otherwise, the user must convert all desired column names to Raven-recognized names, or
#' provide the mapping information as a list through this parameter. An example format can be seen
#' (the mapping used for weathercan by default) in \code{data("rvn_rvt_mappings_data")}.
#'
#' filenames may be used to provide the specific desired filenames (with paths) for each
#' station rvt data file being generated; this should be a character vector of length
#' equal to the number of unique station names in the data.
#'
#' Note that the function uses
#' \code{sort(unique(metdata$STATION_NAME))} to determine the order of stations,
#' thus the filenames should correspond to the sorted vector of station numbers as well.
#'
#' prd is used by the xts formatted-data to restrict the data reported in .rvt
#' files, for each station, to this period. The prd should be defined in
#' "YYYY-MM-DD/YYYY-MM-DD" string format. If the period supplied results in an
#' empty time series (i.e. non-overlapping time periods), an error will be
#' thrown.
#'
#' met_file_prefix can be used to add a prefix to the .rvt data file names, ("met_" by default)
#' which may be useful in organizing multiple climate data files. This is ignored
#' if filenames are specified.
#'
#' write_stndata wil print out the gauge(s) metadata to file (specified by
#' filename_stndata parameter) in the .rvt format, which is required to include a
#' meterological station in Raven. The function will append to the file if it already exists, meaning
#' that this works for iterations of this function. metdata must include the columns
#' ELEV, LAT, and LON if station data is to be written, else the meta data file will not be created.
#'
#' write_redirect will print out the :RedirectToFile commands into the file specified
#' by filename_stndata, if write_stndata is \code{TRUE}. These commands can be copied into the main model's
#' .rvt file to redirect to the produced time series files. The function will
#' append to the file if it already exists, meaning that this works for
#' iterations of this function.
#'
#' The function has several built-in data quality checks. These include:
#'
#' * checking that the time interval is consistent for each station;
#' * ensuring that meta data is unique for each station name; and
#' * check for missing data and issuing a warning that post-processing will be required
#'
#' Data quality is not assessed in this package, such as consistency between
#' minimum and maximum temperatures and missing data. Consider viewing the RavenR.extras package for
#' functions to interpolate missing meteorological data and checking for min/max temperature consistency.
#'
#' This function is designated to use data from the weathercan package, but may be used with any
#' supplied data frame of meteorological information if in the correct format and other
#' relevant information (such as rvt_met_mapping, if needed) is supplied. The
#' weathercan package is external to RavenR and is not an explicit dependency
#' of RavenR, although a sample weathercan data set can be viewed as \code{data(rvn_weathercan_sample)}.
#'
#' @param metdata EC meteorological data from one or more stations (e.g., from \code{weathercan::weather_dl()})
#' @param rvt_met_mapping list that provides the mapping between metdata names and those used in Raven
#' @param filenames (optional) character vector of filenames for the rvt data files, length same as number of stations in metdata
#' @param met_file_prefix (optional) prefixes the file name (default: "met_")
#' @param prd (optional) data period to use in .rvt file
#' @param write_stndata (optional) write the gauge data to a separate .rvt file
#' @param write_redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param filename_stndata (optional) name of the station data file created (if \code{write_stndata=TRUE})
#' @param NA_value (optional) value to use for NA values in rvt file (default -1.2345 for Raven format)
#' @return \item{TRUE}{return \code{TRUE} if the function is executed properly}
#'
#' @seealso \code{\link{rvn_rvt_write}} to write non-forcing time series data to Raven rvt format.
#'
#' Download Environment Canada Historical weather data from (climate.weather.gc.ca), or use the
#' `weathercan` package to access this data through R.
#'
#' @examples
#' # note: example modified to avoid using weathercan directly, uses saved
#' ## weathercan data from RavenR package sample data
#' data(rvn_weathercan_sample)
#' kam <- rvn_weathercan_sample
#'
#' # basic use, override filename to temporary file
#' rvn_rvt_write_met(metdata = kam,
#'   filenames = file.path(tempdir(), "rvn_rvt_metfile.rvt"))
#'
#' @export rvn_rvt_write_met
#' @importFrom xts xts is.timeBased
#' @importFrom lubridate as_datetime
#' @importFrom dplyr filter
#' @importFrom zoo coredata
rvn_rvt_write_met <-  function(metdata, rvt_met_mapping=NULL, filenames=NULL, met_file_prefix='met_', prd=NULL,
                           write_stndata = TRUE, write_redirect=TRUE, filename_stndata = "met_stndata.rvt",
                            NA_value=-1.2345) {

  # data("rvn_rvt_mappings_data")

  ## load parameter mapping lists
  if (is.null(rvt_met_mapping)) {
    # no mappings provided, assume weathercan default mappings
    rvt_met_mapping <- get_rvt_met_mapping_weathercan()

  } else {

    warning("No QA/QC provided for non-RavenR generated mappings. Names compatible with Raven directly will still be used.")
    # provide custom mappings of the format list=c('column_name'=list('Raven_name'))
    # test the provided mappings
    # if (!(class(rvt_met_mapping)) == "list") {
    #   stop("rvt_met_mapping must be of type list if provided. Please see RavenR::rvn_rvt_mappings() for an example of the expected format.")
    # }

    # else {
    #   # other checks to be added
    #   stop("currenly not supported")
    # }
  }

  rvn_met_raven_mapping <- get_rvn_met_raven_mapping()

  colnames(metdata) <- toupper(colnames(metdata)) # prevents any upper/lower case matching issues
  mand_col <- c("STATION_NAME","DATETIME")
  extra_col <- c("LAT","LON","ELEV")

  # expect a column as DATETIME. weathercan supplies DATE and adds TIME if resolution is subdaily
  ## create a new DATETIME column if it does not exist
  if ("DATETIME" %notin% colnames(metdata)) {
    if ("TIME" %in% colnames(metdata)) {
      if (xts::is.timeBased(metdata$TIME)) {
        metdata$DATETIME <- lubridate::as_datetime(metdata$TIME)
        metdata <- metdata[,-which(colnames(metdata)=="TIME")]
        if ("DATE" %in% colnames(metdata)) {
          metdata <- metdata[,-which(colnames(metdata)=="DATE")]
        }
      }
    } else if ("DATE" %in% colnames(metdata)) {
      if (xts::is.timeBased(metdata$DATE)) {
        metdata$DATETIME <- lubridate::as_datetime(sprintf("%s 00:00:00",metdata$DATE))  # ,format="%Y-%m-%d %H:%M:%S"
        metdata <- metdata[,-which(colnames(metdata)=="DATE")]
      }
    } else {
      stop("metdata must have a column called datetime, date or time (date with a time included).")
    }
  }

  ## verify inputs
  if (nrow(metdata) <= 0) {
    stop("Requires at least one line of data")
  }

  ## check column names
  # mandatory columns
  if (any(mand_col %notin% colnames(metdata))) {
    missing_col <- mand_col[!(mand_col %in% colnames(metdata))]
    stop(sprintf("rvn_rvt_write_met: Missing required columns: %s", missing_col))
  }

  # check number of unique station names against filenames
  if (!is.null(filenames)) {
    if (length(filenames) != length(unique(metdata$STATION_NAME))) {
      stop("rvn_rvt_write_met: Length of filenames not equal to the number of unique station names.")
    }
  }

  # take only supported data columns
  metdata <- metdata[,which(colnames(metdata) %in% c(names(rvn_met_raven_mapping),names(rvt_met_mapping),mand_col,extra_col))]
  data_col <- colnames(metdata)[!(colnames(metdata) %in% c(mand_col,extra_col))]

  # standardize column names
  # metdata = metdata[,c(mand_col,data_col)]
  wcn <- names(metdata) %in% names(rvt_met_mapping)
  if(any(wcn)){
    # convert any weathercan parameter names to Raven convention
    colnames(metdata)[colnames(metdata) %in% names(rvt_met_mapping)] <-
      unlist(rvt_met_mapping[data_col])
  }

  # get units for each parameter
  data_col <- colnames(metdata)[!(colnames(metdata) %in% c(mand_col,extra_col))] # update parameter column names
  data_units <- unlist(rvn_met_raven_mapping[data_col])

  # Number of data sets
  if (length(unique(metdata$STATION_NAME))>1) {
    print(paste("Imported data from", length(unique(metdata$STATION_NAME)), "stations"))
  } else {
    print("Imported data from 1 station")
  }

  # Replace spaces in Station Names with underscores
  metdata$STATION_NAME <- gsub(" ", "_", metdata$STATION_NAME)

  # write status of each rvt file. Geographic data is written for every successful station into the main rvt metadata.
  ws <- data.frame("station" = unique(metdata$STATION_NAME), "rvt.name" = NA ,"status" = NA)

  ## Begin writing rvt file for each station listed in entry
  stns <- sort(unique(metdata$STATION_NAME))
  for (i in 1:length(stns)){
    sn <- stns[i]
    rr <- metdata[metdata$STATION_NAME == sn,c(mand_col,extra_col,data_col)]

    # Subset period if necessary
    if (!(is.null(prd))) {
      prd2 <- rvn_get_prd(rr.ts, prd)
      # rr <- filter(rr, rr$date >= as.Date(firstsplit[1]), rr$date <= as.Date(firstsplit[2]))
      rr <- rr[prd2]
    }
    rr.ts <- xts(rr[,data_col], order.by=rr$DATETIME)

    # Verify existing record overlaps with desired period
    if(nrow(rr) == 0 ){
      message(paste0("No data available for desired period at ",sn," (station name: ",sn,")"))
    }

    # check for time interval and consistency
    difftime_check <- difftime(rr.ts[2:nrow(rr.ts),], rr.ts[1:(nrow(rr.ts)-1),] , units="day")
    if (any(difftime_check != difftime_check[1])) {
      stop("Inconsistent timesteps found in data; consider using tools such as RavenR::rvn_ts_infill to fix the time series.")
    }
    time_interval <- difftime_check[1]

    # replace any NA values
    if (any(is.na(rr.ts))){
      rr.ts[is.na(rr.ts)] <-  NA_value # set to Raven default NA value -1.2345
      warning(sprintf("NA values detected for station %s and set to NA value (%.4f) in time series.\nNote that met forcings cannot have NA values in Raven.",sn,NA_value))
    }

    if (is.null(filenames)) {
      if (!is.null(met_file_prefix)) {
        rvt.name <- sprintf("%s%s.rvt", met_file_prefix, sn) # write new file with prefix
      }else{
        rvt.name <- sprintf('%s.rvt',sn) # write new file without prefix
      }
    } else {
      rvt.name = filenames[i]
    }
    ws$rvt.name[ws$station==sn] <- rvt.name

    ## begin writing rvt file
    fc = file(rvt.name,open = "w+")
    # header block
    if(length(data_col)>1){
      # multiple data columns, use :MultiData format
      writeLines(sprintf(":MultiData"),fc)
      writeLines(sprintf("  %s %g %i",format(min(as_datetime(rr.ts)),"%Y-%m-%d %H:%M:%S"),time_interval,nrow(rr.ts)),fc)
      writeLines(sprintf("  :Parameters %s",paste(data_col, collapse = " ")),fc)
      writeLines(sprintf("  :Units %s",paste(data_units, collapse = " ")),fc)
    }else{
      # singular data column, use FORCING_TYPE
      writeLines(sprintf(":Data %s %s",data_col, data_units))
      writeLines(sprintf("  %s %g %i",format(min(as_datetime(rr.ts)),"%Y-%m-%d %H:%M:%S"),time_interval,nrow(rr.ts)))
    }
    # time series
    for (j in 1:nrow(rr.ts)){
      # currently prints to 1 decimal place precision (will need to modify for higher precision)
      writeLines(sprintf("  %s",paste(coredata(rr.ts[j]),collapse=" ")),fc)
    }
    # end block
    if(length(data_col>1)){
      writeLines(":EndMultiData",fc)
    }else{
      writeLines(":EndData",fc)
    }
    close(fc)
    # Report success
    message(sprintf("rvn_rvt_write_met: Done writing to %s", rvt.name))
    ws$status[ws$station == sn] = TRUE
  }

  message(sprintf("rvn_rvt_write_met: Done writing rvt files for %i station(s)", length(stns)))

  # Write station data
  if (write_stndata ){

    if (all(c("STATION_NAME","LAT","LON","ELEV") %in% names(metdata))) {

      ## Write geographic values of stations to main rvt file
      md <-  do.call("rbind",lapply(ws$station[ws$status == TRUE],function(s){
        md <- metdata[metdata$STATION_NAME==s, c("STATION_NAME","LAT","LON","ELEV")]
        md <- data.frame(md[!duplicated(md),], "rvt.name" = ws$rvt.name[ws$station==s])
      }))

      # check that station data is unique
      if (length(unique(md$STATION_NAME)) != nrow(md)) {
        warning("rvn_rvt_write_met: Station metadata is not unique by STATION_NAME; metadata not written to file.")
      } else {
        fc = file(filename_stndata,open = "w+")
        for (k in 1:nrow(md)){
          writeLines(sprintf(":Gauge %s", md$STATION_NAME[k]),fc)
          writeLines(sprintf("  :Latitude %.6f", md$LAT[k]),fc)
          writeLines(sprintf("  :Longitude %.6f", md$LON[k]),fc)
          writeLines(sprintf("  :Elevation %.2f", md$ELEV[k]),fc)
          writeLines(":EndGauge\n",fc)
        }

        ## Write Redirect commands in stndata file
        if(write_redirect) {
          # writeLines("\n",fc)
          # fc.redirect = file(rd_file, open = "a+")
          for(k in 1:nrow(md)){
            writeLines(sprintf(":RedirectToFile %s", md$rvt.name[k]),fc)
          }
        }

        close(fc)
        message(sprintf("rvn_rvt_write_met: Done writing station data to %s", filename_stndata))
      }
    } else {
      warning(sprintf("rvn_rvt_write_met: One or more metadata columns required for writing station data not found.\nFile %s not written to.",filename_stndata))
    }
  }

  return(TRUE)
}
