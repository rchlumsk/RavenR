#' @title Write Raven rvt file from Time Series
#'
#' @description
#' rvn_rvt_write generates a Raven rvt file of the specified type from an xts time series.
#'
#' @details
#' Writes the rvt file for a given time series dataset. The type of rvt file to write is
#' determined by the rvt_type argument, which must match one of the supported Raven types.
#' Note that this function does not support the writing of meteorological data, this is handled
#' by the \code{rvn_rvt_write_met} function.
#'
#' The format of the rvt file, including required fields to write to file, are determined from
#' the supplied rvt_type parameter and from the mapping provided by \code{data("rvn_rvt_mappings_data")}. The
#' data_type is also checked against the provided mappings to check for valid state variables and
#' accompanying units.
#'
#' No quality control of the data is performed here. Some rvt types, such as ObservationWeights,
#' cannot have missing values in the data; it is the responsibility of the user to supply \code{x} with
#' no missing values if required. Any missing values in \code{x} are written to file with the
#' missing value code provided by \code{NA_value}.
#'
#' \code{x} should be an xts time series object with multiple rows of data and a single column.
#'
#' @param x time series in xts format to write to file
#' @param filename name of output file (with rvt extension)
#' @param rvt_type type of rvt file to write (e.g. ObservationData)
#' @param data_type type of data in x (e.g. HYDROGRAPH)
#' @param basin_ID subbasin (or HRU) ID corresponding to the time series
#' @param NA_value value to use for NA values in rvt file (default -1.2345 for Raven format)
#' @return \code{TRUE} if the function executed successfully
#'
#' @seealso \code{\link{rvn_rvt_read}} to read in rvt data files,
#' and \code{rvn_rvt_write_met} to write meteorological rvt files.
#'
#' @examples
#'
#' # load sample flow data
#' system.file('extdata','run1_Hydrographs.csv', package = "RavenR") %>%
#' rvn_hyd_read() -> mydata
#'
#' # write time series to rvt file using data from subbasin 36 as observed data
#' rvn_rvt_write(x=mydata$hyd$Sub36,
#'   rvt_type = "ObservationData",
#'   data_type = "HYDROGRAPH",
#'   basin_ID = 36,
#'   filename = file.path(tempdir(), 'mydata.rvt'))
#'
#' @export rvn_rvt_write
#' @importFrom xts is.xts timeBased
#' @importFrom lubridate as_datetime
rvn_rvt_write <- function(x, filename=NULL, rvt_type="ObservationData",
                          data_type="HYDROGRAPH",
                          basin_ID=NULL, NA_value=-1.2345) {

  # pull in rvt mappings within function
  data("rvn_rvt_mappings_data")

  ### general inputs checks

  if (timeBased(x) || !is.xts(x)) {
    stop("x must be of class xts")
  }

  # check for non-zero length in series
  if (is.null(nrow(x)) | nrow(x) == 0) {
    stop("x must have some data")
  }

  # check for a single column
  if (ncol(x) > 1) {
    warning("x has multiple columns, only the first will be used")
    x <- x[,1]
  }

  if (is.null(filename)) {
    if (is.null(colnames(x))) {
      filename <- sprintf("rvn_rvt_output_%s.rvt",format(Sys.Date()))
      warning(sprintf("No filename provided, auto-generated as %s", filename))
    } else {
      filename <- sprintf("rvn_rvt_%s_%s.rvt",colnames(x), format(Sys.Date()))
      warning(sprintf("No filename provided, auto-generated as %s", filename))
    }
  } else if (rvn_substrRight(filename,4) != ".rvt") {
    warning("Adding .rvt extension to filename")
    filename <- sprintf("%s.rvt",filename)
  }

  # check rvt type
  if (rvt_type %notin% names(rvt_mapping)) {
    stop(sprintf("Unknown rvt_type %s, please ensure the rvt_type is a recognized type:\n%s",rvt_type,
                 paste(names(rvt_mapping), collapse="\n")))
  }

  # list of items that will be written based on the rvt_type
  rvt_writelist <- rvt_mapping[[rvt_type]]

  # check data type (if in the rvt_mapping list)
  if ("data_type" %in% unlist(rvt_writelist)) {
    if (data_type %notin% names(rvt_data_type_mapping)) {
      stop(sprintf("Unknown data_type %s, please ensure the data_type is a recognized type:\n%s",data_type,
                   paste(names(rvt_data_type_mapping), collapse="\n")))
    }
  }

  # similar check for basin_ID
  if (is.null(basin_ID) & "basin_ID" %in% unlist(rvt_writelist)) {
    basin_ID <- 12345
    warning(sprintf("basin_ID is required but not provided, using default placeholder value of %i",basin_ID))
  }

  ### xts data checks

  # check the interval for consistency
  if (length(grep("Irregular", x=rvt_type)) != 1) {
    difftime_check <- difftime(x[2:length(x)], x[1:(length(x)-1)] , units="day")
    if (any(difftime_check != difftime_check[1])) {
      stop("Inconsistent timesteps found in data; consider using tools such as RavenR::rvn_ts_infill to fix the time series.")
    }
    time_interval <- difftime_check[1]
    start_datetime <- format(as_datetime(x[1]), "%Y-%M-%d %H:%M:%S")

  } else {
    time_interval <- NA
  }

  # get other properties of x
  num_points <- nrow(x)

  # change all NA values to NA value
  x[is.na(x)] <- NA_value


  ### write file
  xx <- coredata(x)
  fc <- file(filename,open='w+')

  if (!is.na(time_interval)) {

    # write regular time series (even interval)

    ## write first line
    ss1 <- paste0( c(sprintf(":%s",rvt_type),
                     unlist(rvt_writelist[[1]])),
                   collapse = " ")
    ss1 <- gsub("\\bdata_type\\b", data_type, ss1)
    ss1 <- gsub("\\bbasin_ID\\b", basin_ID, ss1)
    ss1 <- gsub("\\bunits\\b", rvt_data_type_mapping[[data_type]]$units, ss1)
    writeLines(ss1,fc)

    ss2 <- paste0(c("  ",
                    unlist(rvt_writelist[[2]])),
                  collapse = " ")
    ss2 <- gsub("\\bstart_datetime\\b", start_datetime, ss2)
    ss2 <- gsub("\\btime_interval\\b", time_interval, ss2)
    ss2 <- gsub("\\bnum_points\\b", num_points, ss2)
    writeLines(ss2,fc)

    for (j in 1:num_points) {
      writeLines(sprintf('  %g',xx[j]),fc)
    }

  } else {

    # write irregular time series

    ## write first line
    ss1 <- paste0(c( sprintf(":%s",rvt_type),
                     unlist(rvt_writelist[[1]])),
                  collapse = " ")
    ss1 <- gsub("\\bdata_type\\b", data_type, ss1)
    ss1 <- gsub("\\bbasin_ID\\b", basin_ID, ss1)
    ss1 <- gsub("\\bnum_points\\b", num_points, ss1)
    ss1 <- gsub("\\bunits\\b", rvt_data_type_mapping[[data_type]]$units, ss1)
    writeLines(ss1,fc)

    for (j in 1:num_points) {
      writeLines(sprintf('  %s %g', format(as_datetime(x[j]), "%Y-%M-%d %H:%M:%S"),xx[j]),fc)
    }
  }

  # close off file
  writeLines(sprintf(':End%s',rvt_type),fc)
  close(fc)

  return(TRUE)
}
