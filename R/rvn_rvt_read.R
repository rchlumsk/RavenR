#' @title Read .rvt (Raven time series) file
#'
#' @description
#' This routine reads in a valid Raven time series input (.rvt) file and returns the
#' information as an xts time series and metadata data frame.
#'
#' @details
#' All rvt data types available in Raven are supported (e.g. :MultiData, :Data, :ObservedData,
#' :BasinInflowHydrograph). This is handled by the mappings provided in the \code{data("rvn_rvt_mappings_data")}
#' function, and additional support for other rvt types can likely be included by making updates
#' to that function with modifications to the somewhat generic read function here.
#'
#' It does NOT support the master .rvt file with :Gauge or :GriddedForcing commands, only the
#' reading of time-series based .rvt files with a single time series block within the file.
#'
#' The timezone is provided by the tzone argument as "UTC" by default, and should be adjusted by
#' the user to the local time zone as needed, based on the model run.
#'
#' @param filename path/name of the .rvt file (with .rvt extension included)
#' @param tzone string indicating the timezone of the data provided in filename (default "UTC")
#'
#' @return
#' Returns a list with two objects:
#'   \item{rvt_xts}{xts formatted time series with data}
#'   \item{rvt_metadata}{data frame with a 'param' and 'value' column providing rvt metadata (number of points, time interval, subbasin ID, etc.)}
#'
#' @examples
#' # read in rvt file
#' system.file('extdata','GlenAllan.rvt',package="RavenR") %>%
#' rvn_rvt_read(.) -> rvt
#' plot(rvt$rvt_xts$TEMP_DAILY_MIN)
#'
#' @export rvn_rvt_read
#' @importFrom xts xts
rvn_rvt_read<-function(filename, tzone="UTC") {

  stopifnot(file.exists(filename))

  ff <- readLines(filename)

  if (length(grep(":Gauge", ff)!=0) | length(grep(":GriddedForcing", ff)!=0)){
    print("rvn_rvt_read does not support reading of master model .rvt files, only data files ")
  }

  # pull in rvt mappings within function
  # data("rvn_rvt_mappings_data")
  rvt_mapping <- get_rvt_mapping()
  rvn_met_raven_mapping <- get_rvn_met_raven_mapping()
  rvt_data_type_mapping <- get_rvt_data_type_mapping()

  # determine which file type is being read
  for (nn in names(rvt_mapping)) {
    mult <- grep(sprintf("\\b:%s\\b",nn), ff)
    if (!length(mult)==0) {
      rvt_type <- nn
      if (length(mult) > 1) {
        stop(sprintf("Multiple instances of :%s found in file, data should be stored in separate files.",nn))
      }
      break
    }
  }

  # start reading file
  startfile<-grep(sprintf("\\b:%s\\b",rvt_type), ff, value = FALSE)[1]
  endfile<-grep(sprintf("\\b:End%s\\b",rvt_type), ff, value = FALSE)[1]

  rvt_readlist <- rvt_mapping[[rvt_type]]
  # modify readlist to split start_datetime into date and time
  if ("start_datetime" %in% rvt_readlist[[1]]) {
    ind <- grep("start_datetime",rvt_readlist[[1]])
    rvt_readlist[[1]] <- c(rvt_readlist[[1]][1:ind],rvt_readlist[[1]][ind:length(rvt_readlist[[1]])])
    rvt_readlist[[1]][ind] <- "start_date"
    rvt_readlist[[1]][(ind+1)] <- "start_time"
  }

  if (length(rvt_readlist) >=2) {
    if ("start_datetime" %in% rvt_readlist[[2]]) {
      ind <- grep("start_datetime",rvt_readlist[[2]])
      rvt_readlist[[2]] <- c(rvt_readlist[[2]][1:ind],rvt_readlist[[2]][ind:length(rvt_readlist[[2]])])
      rvt_readlist[[2]][ind] <- "start_date"
      rvt_readlist[[2]][(ind+1)] <- "start_time"
    }
  }

  # start storing rvt data
  if (rvt_type == "MultiData") {

    meta_df <- data.frame(matrix(NA,ncol=2,nrow=length(unlist(rvt_readlist))))
    colnames(meta_df) <- c("param","value")
    meta_df$param <- unlist(rvt_readlist)

    linecount <- 1
    j <- startfile

    while (j < (endfile-startfile)) {

      temp <- unlist(strsplit(trimws(ff[j]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

      # take only temp items before a comment char
      if (any(rvn_substrLeft(temp,1) == "#")) {
        if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
          temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
        } else {
          temp <- NULL
        }
      }

      if (is.null(temp)) {
        j <- j+1 # comment encountered, increment j only
      } else {

        if (linecount == 1) {

          if (temp[1] != ":MultiData") {
            stop(sprintf("rvn_rvt_read: Error in reading :MultiData line on line %i",j))
          }

          j <- j+1
          linecount <- linecount +1

        } else if (linecount == 2) {

          if ((length(temp)) == length(rvt_readlist[[2]])) {
            for (i in 1:length(rvt_readlist[[2]])) {
              meta_df[meta_df$param == unlist(rvt_readlist[[2]])[i],]$value <- temp[i]
            }
          } else {
            stop(sprintf("rvn_rvt_read: Error reading rvt file, format on line %i not consistent with anticipated Raven format.",j))
          }

          j <- j+1
          linecount <- linecount +1

        } else if (linecount == 3) {

          if (temp[1] != ":Parameters") {
            stop(sprintf("rvn_rvt_read: Error reading rvt file, format on line %i not consistent with anticipated Raven format.",j))
          } else {
            mult_params <- temp[2:(length(temp))]

            # check mult_params against Raven rvt list
            if (any(mult_params %notin% names(rvn_met_raven_mapping))) {
              stop(sprintf("rvn_rvt_read: Error reading rvt file, parameters found in rvt file that are not recognized in Raven on line %i:\n%s.",j,
                           paste(mult_params[which(mult_params %notin% names(rvn_met_raven_mapping))],collapse=" ")))
            }

            meta_df[meta_df$param == ":Parameters", "value"] <- paste(mult_params, collapse=",")
          }

          j <- j+1
          linecount <- linecount +1

        } else if (linecount == 4) {

          if (temp[1] != ":Units") {
            stop(sprintf("rvn_rvt_read: Error reading rvt file, format on line %i not consistent with anticipated Raven format.",j))
          } else {
            mult_units <- temp[2:(length(temp))]

            # check length of mult_units against mult_params
            if (length(mult_units) != length(mult_params)) {
              warning(sprintf("rvn_rvt_read: Issue in reading rvt file, number of units on line %i not consistent with number of parameters specified.",j))
            }

            # check mult_units against Raven rvt list
            # to be added - check units for each parameter against those in the rvt mapping
            if (any(tolower(mult_units) != tolower(unlist(rvn_met_raven_mapping[mult_params])))) {
              warning(sprintf("rvn_rvt_read: Issue in reading rvt file, some units on line %i found that are not consistent with the associated parameters, please verify that all data is in the correct units for Raven:\n%s",j,
                           paste(mult_units[which(tolower(mult_units) != tolower(unlist(rvn_met_raven_mapping[mult_params])))],collapse=" ")))
            }

            meta_df[meta_df$param == ":Units", "value"] <- paste(mult_units, collapse=",")
          }

          j <- j+1
          linecount <- linecount +1

        } else {

          # print(sprintf("linecount is now %i", linecount))

          # create the data matrix based on the number of parameters that get read in
          dd <- matrix(NA,ncol=length(mult_params),nrow=(endfile-j))
          colnames(dd) <- mult_params

          for (i in j:(endfile-1)) {
            temp <- unlist(strsplit(trimws(ff[i]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

            # take only temp items before a comment char
            if (any(rvn_substrLeft(temp,1) == "#")) {
              if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
                temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
              } else {
                temp <- NULL
              }
            }

            if (!is.null(temp)) {
              if (length(temp) == length(mult_params)) {
                dd[(i-j+1),] <- as.numeric(temp)
              } else {
                stop(sprintf("rvn_rvt_read: Error reading rvt file, number of expected values on line %i not consistent with number of declared parameters.",j))
              }
            }
          }

          if ("num_points" %in% meta_df$param) {
            if (nrow(dd) != meta_df[meta_df$param=="num_points","value"]) {
              stop("rvn_rvt_read: Number of points not consistent with header declaration;\nPlease check for file consistency and runaway comment characters.")
            }
          }

          dd.xts <- xts(dd, order.by=seq.POSIXt(from=as.POSIXct(sprintf("%s %s",
                                                                        meta_df[meta_df$param=="start_date","value"],
                                                                        meta_df[meta_df$param=="start_time","value"]),
                                                                tz=tzone),
                                                by=as.numeric(meta_df[meta_df$param=="time_interval","value"])*86400,
                                                length.out = as.numeric(meta_df[meta_df$param=="num_points","value"])))
          # names(dd.xts) <- rvt_type # names already provided to dd
          break
        }
      }
    }

  }  else if (length(grep("\\bIrregular", rvt_type)) ==1) {

    meta_df <- data.frame(matrix(NA,ncol=2,nrow=length(unlist(rvt_readlist))))
    colnames(meta_df) <- c("param","value")
    meta_df$param <- unlist(rvt_readlist)

    dd <- matrix(NA,ncol=1,nrow=(endfile-startfile))

    linecount <- 1
    j <- startfile

    while (j < (endfile-startfile)) {

      temp <- unlist(strsplit(trimws(ff[j]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

      # take only temp items before a comment char
      if (any(rvn_substrLeft(temp,1) == "#")) {
        if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
          temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
        } else {
          temp <- NULL
        }
      }

      if (is.null(temp)) {
        j <- j+1 # comment encountered, increment j only
      } else {

        if (linecount == 1) {

          if ((length(temp)-1) == length(rvt_readlist[[1]])) {
            for (i in 1:length(rvt_readlist[[1]])) {
              meta_df[meta_df$param == unlist(rvt_readlist[[1]])[i],]$value <- temp[i+1]
            }
          } else {
            stop(sprintf("rvn_rvt_read: Error reading rvt file, format on line %i not consistent with anticipated Raven format.",j))
          }

          j <- j+1
          linecount <- linecount +1

        } else {

          dd <- data.frame(matrix(NA,ncol=3,nrow=(endfile-j)))
          colnames(dd) <- c("DATE","TIME","VALUE")

          for (i in j:(endfile-1)) {
            temp <- unlist(strsplit(trimws(ff[i]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

            # take only temp items before a comment char
            if (any(rvn_substrLeft(temp,1) == "#")) {
              if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
                temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
              } else {
                temp <- NULL
              }
            }

            if (!is.null(temp)) {
              if (length(temp) == 3) {
                dd[(i-j+1),] <- temp
              } else {
                stop(sprintf("rvn_rvt_read: Error reading rvt file, number of expected values on line %i not consistent with number of declared parameters.",j))
              }
            }
          }

          # convert VALUE column to numeric
          dd$VALUE <- as.numeric(dd$VALUE)

          # remove NA
          dd <- dd[seq(1,nrow(dd))[!apply(dd,1,function(x) any(is.na(x)))],]

          if ("num_points" %in% meta_df$param) {
            if (nrow(dd) != meta_df[meta_df$param=="num_points","value"]) {
              stop("rvn_rvt_read: Number of points not consistent with header declaration;\nPlease check for file consistency and runaway comment characters.")
            }
          }

          dd.xts <- xts(dd$VALUE, order.by=as.POSIXct(sprintf("%s %s",
                                                              dd$DATE,
                                                              dd$TIME),
                                                              tz=tzone))
          names(dd.xts) <- rvt_type
          break
        }
      }
    }

  } else {

    meta_df <- data.frame(matrix(NA,ncol=2,nrow=length(unlist(rvt_readlist))))
    colnames(meta_df) <- c("param","value")
    meta_df$param <- unlist(rvt_readlist)

    dd <- matrix(NA,ncol=1,nrow=(endfile-startfile))

    linecount <- 1
    j <- startfile

    while (j < (endfile-startfile)) {

      temp <- unlist(strsplit(trimws(ff[j]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

      # take only temp items before a comment char
      if (any(rvn_substrLeft(temp,1) == "#")) {
        if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
          temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
        } else {
          temp <- NULL
        }
      }

      if (is.null(temp)) {
        j <- j+1 # comment encountered, increment j only
      } else {

        if (linecount == 1) {

          if ((length(temp)-1) == length(rvt_readlist[[1]])) {
            for (i in 1:length(rvt_readlist[[1]])) {
              meta_df[meta_df$param == unlist(rvt_readlist[[1]])[i],]$value <- temp[i+1]
            }
          } else {
            stop(sprintf("rvn_rvt_read: Error reading rvt file, format on line %i not consistent with anticipated Raven format.",j))
          }

          j <- j+1
          linecount <- linecount +1

        } else if (linecount == 2) {

          if ((length(temp)) == length(rvt_readlist[[2]])) {
            for (i in 1:length(rvt_readlist[[2]])) {
              meta_df[meta_df$param == unlist(rvt_readlist[[2]])[i],]$value <- temp[i]
            }
          } else {
            stop(sprintf("rvn_rvt_read: Error reading rvt file, format on line %i not consistent with anticipated Raven format.",j))
          }

          j <- j+1
          linecount <- linecount +1

        } else {

          # print(sprintf("linecount is now %i", linecount))

          temp_all <- unlist(strsplit(trimws(ff[j:(endfile-1)]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

          # check temp outputs for single entries only, once comments removed
          for (i in length(temp_all)) {

            temp <- temp_all[i]

            # take only temp items before a comment char
            if (any(rvn_substrLeft(temp,1) == "#")) {
              if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
                temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
              } else {
                temp <- NULL
              }
            }

            if (length(temp) >1) {
              stop(sprintf("rvn_rvt_read: Too many items on line %i of rvt file",i))
            }
          }

          dd <- as.numeric(temp_all)
          dd <- dd[!is.na(dd)]

          # note: any Raven NA of -1.2345 or other flags should still be numerical and not NA when read from the file
          ## therefore assume that any NA from the script is a comment, and switching NA values to -1.2345 would not make
          ## sense to do, as it would likely introduce more entries than anticipated

          if ("num_points" %in% meta_df$param) {
            if (length(dd) != meta_df[meta_df$param=="num_points","value"]) {
              stop("rvn_rvt_read: Number of points not consistent with header declaration;\nPlease check for file consistency and runaway comment characters.")
            }
          }

          dd.xts <- xts(dd, order.by=seq.POSIXt(from=as.POSIXct(sprintf("%s %s",
                                                                        meta_df[meta_df$param=="start_date","value"],
                                                                        meta_df[meta_df$param=="start_time","value"]),
                                                                tz=tzone),
                                                by=as.numeric(meta_df[meta_df$param=="time_interval","value"])*86400,
                                                length.out = as.numeric(meta_df[meta_df$param=="num_points","value"])))
          names(dd.xts) <- rvt_type
          break
        }
      }
    }
  }

  return(list(
    "rvt_xts"=dd.xts,
    "rvt_metadata"=meta_df
  ))

}
