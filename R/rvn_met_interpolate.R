#' @title Interpolate meteorological data using IDW
#'
#' @description
#' Interpolates/infills missing meteorological data by
#' using an inverse-distance weighting scheme to infill using data from nearby
#' meteorological stations; issues where maximum temperature is less than minimum
#' temperature can also be resolved.
#'
#' @details
#' This function takes a meteorological data set with multiple station data in one data frame and
#' interpolates the missing values for key stations, for the specified meteorological variables to interpolate.
#'
#' The format of the weather_data input is consistent with that from the \code{weathercan::weather_dl}
#' function, which is the recommended tool to gather this input (see the examples).
#'
#' This function does not guarantee to infill all missing values, since this depends on the availability of data
#' at other locations when it lacks at a given station, although a warning is issued if
#' missing values remain following the interpolation. It may be possible to infill missing
#' values through repeated uses of this function, although the information will be less robust and
#' more reliant on fewer stations with repeated calls of this function.
#'
#' This function does not (currently) perform checks for the quality of the supplied or infilled data,
#' such as checking for maximum temperature less than minimum temperature, unreasonable precipitation values, etc.
#'
#' The key_stn_ids indicates which stations should have their missing values interpolated. It is likely that
#' the user will require more data to perform a proper interpolation than the user cares to have gauge
#' records at, i.e. some stations are only provided for the purposes of infilling missing data at other stations.
#' Since the interpolation of all stations provided can become computationally expensive, the user can
#' specify which stations they want to interpolate data for with the key_stn_ids parameter. Station IDs that
#' are not within the key_stn_ids (if not NULL) will still be used, but not themselves infilled.
#'
#' fix_interp_temp is a boolean for the function to swap any max_temp and min_temp fields where the maximum
#' temperature is less than the minimum temperature, a common fix in messy environmental data. This function
#' will only apply if the columns \code{max_temp} and \code{min_temp} are present, and will only apply to the
#' interpolated fields. By default this is enabled, and automatic fixes to interpolated data will be done;
#' supplied data where the max temp is less than min temp will not be fixed unless fix_base_temp is enabled.
#'
#' The distance calculation, estimating the distance between stations, is performed using the
#' \code{\link{rvn_dist_lonlat}} function, which is based on the \code{geosphere} package.
#'
#' @param weather_data data frame of input meteorological data from multiple stations
#' @param cc columns from weather_data to infill missing values in
#' @param key_stn_ids station IDs in which to perform the interpolation
#' @param ppexp exponent to use in inverse distance weighting calculation (default 2)
#' @param fix_interp_temp function will swap interpolated min and max temp if appropriate
#' @param fix_base_temp function will swap any min and max temp if appropriate
#'
#' @return \item{new_wd}{infilled meteorological data set}
#'
#' @seealso \code{\link{rvn_rvt_write_met}} for writing meteorological data sets to rvt format.
#'
#' @examples
#'
#' \dontrun{
#'
#' # example to create infilled data sets
#'
#' library(weathercan)
#' stn <- weathercan::stations_search(name="Glen allan", interval = "day")
#' dl_stn <- stn
#' all_stns <- weathercan::stations_search(coords=c(stn$lat, stn$lon), dist=40,
#'           interval="day", starts_latest = 2002,
#'           ends_earliest = 2010)
#'
#' weather_data <- weather_dl(station_ids = all_stns$station_id, start = "2002-10-01", interval="day")
#' dl_stn <- all_stns[c(1,3,4,6,7),]
#'
#' new_wd <- rvn_met_interpolate(weather_data = weather_data, key_stn_ids = dl_stn$station_id)
#'
#' # some missing values still exist - could re-run the script to infill these values again
#' new_wd2 <- rvn_met_interpolate(weather_data = new_wd, key_stn_ids = dl_stn$station_id)
#' }
#'
#' @export rvn_met_interpolate
rvn_met_interpolate <- function(weather_data=NULL,
                                cc=c("max_temp","min_temp","total_precip"),
                                key_stn_ids=NULL,
                                ppexp=2,
                                fix_interp_temp=TRUE,
                                fix_base_temp=FALSE) {

  wd <- weather_data

  # new wd - subset of keystns
  if (is.null(key_stn_ids)) {
    new_wd <- weather_data
  } else {
    new_wd <- weather_data[weather_data$station_id %in% key_stn_ids,]
  }

  # main interpolation loop
  for (item in cc) {

    for (i in 1:nrow(new_wd)) {

      if (is.na(new_wd[i,item])) {

        currentdate <- new_wd$date[i]

        # temp <- wd[which(wd$date == currentdate),]
        temp <- wd[which(wd$date == currentdate & !(is.na(wd[,item]))),]

        if (nrow(temp) == 0) {
          warning("missing data for %s, more stations may be required: ", currentdate)
        } else if (nrow(temp) == 1) {
          new_wd[i, item] <- temp[1, item]
        } else {

          temp$dists <- rvn_dist_lonlat(matrix(c(temp$lon,temp$lat), nrow=nrow(temp), ncol=2),
                                t(matrix(c(wd$lon[i], wd$lat[i]), nrow=2, ncol=nrow(temp))))

          numer <- 0
          denom <- 0

          for (j in 1:nrow(temp)) {
            numer <- numer + temp[j, item]/(temp[j, "dists"]^ppexp)
            denom <- denom + 1/(temp[j, "dists"]^ppexp)
          }

          new_wd[i, item] <- numer/denom
        }

      }
    }
  }

  # check for NA values in key columns
  if (any(is.na(new_wd[,cc]))) {
    warning("rvn_met_interpolate: Some key columns still contain NA values, additional data, interpolation or re-application of rvn_met_interpolate may required.")
  }

  # check for and fix max_temp < min_temp if permitted
  if ("max_temp" %in% cc & "min_temp" %in% cc) {
    if (any(new_wd$max_temp < new_wd$min_temp)) {

      # determine indices of rows where max temp < min temp
      ind_base <- which(wd$max_temp < wd$min_temp)
      ind_interp <- which(new_wd$max_temp < new_wd$min_temp)
      ind_interp <- ind_interp[which(ind_interp %notin% ind_base)]

      if (fix_interp_temp & length(ind_interp) >0) {
        new_wd[ind_interp,c("max_temp","min_temp")] <-
          new_wd[ind_interp,c("min_temp","max_temp")]
        print(sprintf("rvn_met_interpolate: Fixed %i interpolated rows where max_temp < min_temp",length(ind_interp)))
      }

      if (fix_base_temp & length(ind_base) >0) {
        new_wd[ind_interp,c("max_temp","min_temp")] <-
          new_wd[ind_interp,c("min_temp","max_temp")]
        print(sprintf("rvn_met_interpolate: Fixed %i base rows where max_temp < min_temp",length(ind_base)))
      }

      if (any(new_wd$max_temp < new_wd$min_temp)) {
        ind_all <- which(new_wd$max_temp < new_wd$min_temp)

        if (length(ind_all)>0) {
          warning(sprintf("rvn_met_interpolate: There are %i rows where max_temp < min_temp.",length(ind_all)))
        }
      }
    }
  }

  return(new_wd)
}
