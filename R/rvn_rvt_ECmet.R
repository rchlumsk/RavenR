#' @title EC Climate Station File Conversion
#'
#' @description
#' rvn_rvt_met converts meteorological data for a given station into the
#' .rvt format usable in Raven.
#'
#' @details
#' Writes data in either :Data or :MultiData format depending on the number of
#' supported parameter columns provided. The data should be downloaded and prepared
#' with missing days included. The download website is linked below.
#'
#' The function will write to name generated from the station name, otherwise
#' the .rvt filename may be specified with the filename argument (full path to
#' the filename, including .rvt extension).
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
#' (THE FOLLOWING PARAGRAPH IS SUPERCEDED BY THE NEXT - NEEDS REVIEW)
#' forcing_set specifies the set of forcings to print to .rvt file. Currently
#' there are only two available sets. A value of 1 prints total precipitation,
#' and 2 splits the precipitation into rainfall and snowfall. In some cases the
#' EC data provides only total precipitation, which is a good check to make for
#' the particular climate station before printing rvt files. Both sets
#' currently print max and min daily temperature. Future extensions to this
#' function may provide more options for forcing sets.
#'
#' In some cases the EC data splits total precipitation into rainfall and snowfall,
#' and in others it only provides total preciptation - met rvt files should provide
#' precipitation data either explicitly as 'RAINFALL' and 'SNOWFALL' timeseries, or
#' simply as "TOTAL_PRECIP" (with the option of pairing "SNOW_FRAC").
#'
#' write_redirect will print out the :RedirectToFile commands in a separate
#' file, met_redirects.rvt. These commands can be copied into the main model's
#' .rvt file to redirect to the produced time series files. The function will
#' append to the file if it already exists, meaning that this works for
#' iterations of this function.
#'
#' write_stndata wil print out the gauge metadata to file (met_stndata.rvt) in
#' the .rvt format, which is required to include a meterological station in
#' Raven. The function will append to the file if it already exists, meaning
#' that this works for iterations of this function.
#'
#' perform.qc is currently under construction and is not yet available; setting
#' to TRUE will result in an warning.
#'
#' The function has several built-in data quality checks. These include:
#'
#' * checking that all supplied files are for the same climate station
#' * ensuring;
#' the timestep (data resolution) is the same in each file
#' * automatically;
#' combining time series and ensuring there are no gaps in the data supplied
#' (i.e. time gaps, not missing values);  and
#' * check for missing data and issuing a warning that post-processing will be required
#'
#' Note: Data quality is not assessed in this package, such as consistency between
#' minimum and maximum temperatures. Subdaily data is not currenty supported.
#'
#' Note: this function is designated to use data from the weathercan package. The
#' weathercan package is external to RavenR and is not an explicit dependency
#' of RavenR.
#'
#' @param metdata EC meteorological data from one or more stations (e.g., from weathercan::weather_dl())
#' @param filename specified name of file to write to (optional)
#' @param prd (optional) data period to use in .rvt file
#' @param stnName (optional) station name to use (instead of name in file)
#' @param prefix (optional) prefixes the file name (default: "met_")
#' @param write_redirect (optional) write the :RedirectToFile commands in a
#' separate .rvt file
#' @param write_stndata (optional) write the gauge data to a separate .rvt file
#' @param rd_file (optional) name of the redirect file created (if write_redirect = TRUE)
#' @param stndata_file (optional) name of the station data file created (if write_stndata = TRUE)
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#'
#' @seealso \code{\link{rvn_rvt_wsc}} to convert WSC flow gauge data to Raven format
#'
#' Download Environment Canada Historical weather data from (climate.weather.gc.ca), or use the
#' `weathercan` package to access this data through R.
#'
#' @examples
#'
#' # note: example modified to avoid using weathercan directly, uses saved
#' ## weathercan data from RavenR package sample data
#' data(rvn_weathercan_sample)
#' kam <- rvn_weathercan_sample
#'
#' # basic use, override filename to temporary file
#' rvn_rvt_met(metdata = kam,
#'   filename = file.path(tempdir(), "rvn_rvt_ECmetfile.rvt"))
#'
#' @export rvn_rvt_met
#' @importFrom xts xts
#' @importFrom dplyr filter
#' @importFrom ggplot2 autoplot theme element_blank
rvn_rvt_met <-  function(metdata, filename=NULL, prd = NULL, stnName = NULL, prefix = 'met_',
                           write_redirect = FALSE, write_stndata = FALSE, rd_file = "met_redirects.rvt",
                           stndata_file = "met_stndata.rvt") {

  ## load parameter mapping lists
  rvn_weathercan_mapping()
  rvn_met_data_mapping()
  colnames(metdata) <- toupper(colnames(metdata)) # prevents any upper/lower case matching issues
  mand_col = c("STATION_NAME","LAT","LON","ELEV","DATE")

  ## verify inputs
  if (nrow(metdata) <= 0) {
    stop("Requires at least one line of data")
  }
  ## check column names
  # mandatory columns
  if(sum(mand_col %in% colnames(metdata)) < 5){
    missing_col <- mand_col[!(mand_col %in% colnames(metdata))]
    stop(sprintf("\nmissing required columns: %s", missing_col))
  }

  # supported data columns
  data_col <- colnames(metdata)[!(colnames(metdata) %in% mand_col)]
  ncheck <- sum(data_col %in% names(rvn_met_data_mapping)) + sum(data_col %in% names(rvn_weathercan_mapping))
  if (ncheck != length(data_col)){
    ns_col <- data_col[!(data_col %in% c(names(rvn_weathercan_mapping),names(rvn_met_data_mapping)))]
    # here we assume all non-weathercan derived parameter columns are already properly formatted -
    # (i.e., keep only supported columns)
    data_col <- data_col[!(data_col %in% ns_col)]
    warning("Some data columns are not supported. See ?rvn_rvt_met for details.")
  }

  # standardize column names
  metdata = metdata[,c(mand_col,data_col)]
  wcn <- data_col %in% names(rvn_weathercan_mapping)
  if(any(wcn)){
    # convert any weathercan parameter names to Raven convention
    colnames(metdata)[colnames(metdata) %in% names(rvn_weathercan_mapping)] <-
      unlist(rvn_weathercan_mapping[data_col])
  }

  # get units for each parameter
  data_col <- colnames(metdata)[colnames(metdata)!=mand_col] # update parameter column names
  data_units <- unlist(rvn_met_data_mapping[data_col])

  # Number of data sets
  if (length(unique(metdata$STATION_NAME))>1) {
    print(paste("Imported data from", length(unique(metdata$STATION_NAME)), "stations"))
  } else {
    print("Imported data from 1 station")
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

  # Make sure dates are of Date Class
  metdata$DATE <- as.Date(metdata$DATE)
  if (is.na(max(metdata$DATE))) {
    stop('date column not formated properly (e.g. "%Y-%m-%d")')
  }

  # check for correct time intervals (daily only) and timestamp gaps
  dd <- metdata$DATE # as.Date(format(as.POSIXct(rr$date, format = "%Y-%m-%d %H:%M:%S"), format = "%Y-%m-%d"))
  difftime_check <- difftime(dd[2:length(dd)], dd[1:(length(dd)-1)] , units="day")

  if ("time" %in% colnames(metdata)){ # catches weathercan data supplied at hourly intervals
    stop("\nSome weathercan-derived data appear to be at hourly intervals. Please re-supply these data at daily intervals.")
  }

  if(any(difftime_check != 1)) {
    # Can't it handle if dt > 1? Also, should we not have the user pass this? --> monthly data format support to come later.
    stop(sprintf(
      "\nIrregular time gaps detected: Data should be supplied at daily intervals.\nConsider using tools such as RavenR::rvn_ts_infill to fix the time series."))
  }

  # Replace spaces in Station Names with underscores
  metdata$STATION_NAME <- gsub(" ", "_", metdata$STATION_NAME)

  # write status of each rvt file. Geographic data is written for every successful station into the main rvt metadata.
  ws <- data.frame("station" = unique(metdata$STATION_NAME), "rvt.name" = NA ,"status" = NA)

  ## Begin writing rvt file for each station listed in entry
  for (i in 1:length(unique(metdata$STATION_NAME))){
    rr <- metdata[metdata$STATION_NAME == unique(metdata$STATION_NAME)[i],c(mand_col,data_col)]
    sn <- unique(rr$STATION_NAME)
    #sid <- unique(rr$station_id) # keep only station name as identifier.

    # Subset period if necessary
    if (!(is.null(prd))) {
      rr <- filter(rr, rr$date >= as.Date(firstsplit[1]), rr$date <= as.Date(firstsplit[2]))
    }

    # Verify existing record overlaps with desired period
    if(nrow(rr) == 0 & !is.null(prd)){
      message(paste0("No data available for desired period at ",sn," (station name: ",sn,")"))
    }

    # create time series for extracted parameters
    rr.ts <- xts(rr[,data_col], order.by = dd)
    if (any(is.na(rr.ts))){
      rr.ts[is.na(rr.ts)] = NA_value # set to Raven default NA value -1.2345
      warning("NA values detected and set to Raven NA (-1.2345) in time series - note that met forcings cannot have NA values.")
    }

    # check file names and name convention preferences
    if(!is.null(stnName)){
      if(length(stnName != length(unique(metdata$station_id)))){
        stop("Number of assigned station names does not match number of input stations.")
      }else{
      sn <- stnName[i]
      }
    }

    if (!is.null(filename)) {
      rvt.name <- filename # write to existing file
    }else if(!is.null(prefix)) {
      rvt.name <- sprintf("%s%s.rvt", prefix, sn) # write new file with prefix
    }else{
     rvt.name <- sprintf('%s.rvt',sn) # write new file without prefix
    }
    ws$rvt.name[ws$station==sn] <- rvt.name

    # -- Modified to use rvn_rvt_write --> decoupled to support standalone :MultiData format
    #-- Data sets are subset & converted to dataframe prior to writing (rvn_rvt_write currently doens't handle tibbles)
    #   Dates are seperated out since we are not passing an XTS object

    #if(forcing_set == 1){
    #  rvn_rvt_write(ts = rr.ts,
    #                ff = rvt.name,
    #                params = ":Parameters\tPRECIP\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN",
    #                units  = ":Units\tmm/d\tC\tC")
    #} else {
    #  rvn_rvt_write(ts = rr.ts,
    #                ff = rvt.name,
    #                params = ":Parameters\tRAINFALL\tSNOWFALL\tTEMP_DAILY_MAX\tTEMP_DAILY_MIN",
    #                units  = ":Units\tmm/d\tmm/d\tC\tC")
    #}

    # verify and write station meta-data
    sd = rr[,mand_col] %>% select(.,!("DATE"))
    if(sum(!(duplicated(sd)))>1){
      stop("More than 1 set of meta-data exist for this station: ")
    }

    ## begin writing rvt file
    fc = file(ws$rvt.name,open = "w+")
    # header block
    if(length(data_col)>1){
      # multiple data columns, use :MultiData format
      writeLines(sprintf(":MultiData"),fc)
      writeLines(sprintf("  %s 1.0 %i",format(min(as_datetime(rr.ts)),"%Y-%m-%d %H:%M:%S"),nrow(rr.ts)),fc)
      writeLines(sprintf(":Parameters %s",paste(data_col, collapse = " ")),fc)
      writeLines(sprintf(":Units %s",paste(data_units, collapse = " ")),fc)
    }else{
      # singular data column, use FORCING_TYPE
      writeLines(sprintf(":Data %s %s",data_col, data_units))
      writeLines(sprintf("  %s 1.0 %i",format(min(as_datetime(rr.ts)),"%Y-%m-%d %H:%M:%S"),nrow(rr.ts)))
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
    message(sprintf("Done writing to %s", rvt.name))
    ws$status[ws$station == sn] = TRUE

    # Provide a visual of generated time series
    print("Overview of generated timeseries")
    print(autoplot(rr.ts, geom="line", main = sn)+rvn_theme_RavenR()+theme(axis.title.x = element_blank()))
  }

  ## Write geographic values of stations to main rvt file
  md <-  do.call("rbind",lapply(ws$station[ws$status == TRUE],function(s){
    md <- metdata[metdata$STATION_NAME==s, c("STATION_NAME","LAT","LON","ELEV")]
    md <- data.frame(md[!duplicated(md),], "rvt.name" = ws$rvt.name[ws$station==s])
  }))

  # Write station data
  if(write_stndata){
    fc = file(stndata_file,open = "a+")
    for (k in 1:nrow(md)){
      writeLines(sprintf(":Gauge %s", md$STATION_NAME[k]),fc)
      writeLines(sprintf("  :Latitude %.6f", md$LAT[k]),fc)
      writeLines(sprintf("  :Longitude %.6f", md$LON[k]),fc)
      writeLines(sprintf("  :Elevation %.2f", md$ELEV[k]),fc)
      writeLines(":EndGauge\n",fc)
    }
    close(fc)
    message(sprintf("Done writing station data to %s", stndata_file))
  }

  ## Write Redirect commands
  if(write_redirect){
  fc.redirect = file(rd_file, open = "a+")
   for(k in 1:nrow(md)){
     writeLines(sprintf(":RedirectToFile %s", md$rvt.name[k]),fc.redirect)
   }
  close(fc.redirect)
  message(sprintf("Done writing redirect commands to %s",rd_file))
  }

  return(TRUE)
}
