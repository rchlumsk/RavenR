#' @title Rewrite rvp file with placeholder values
#'
#' @description
#' Rewrites a Raven rvp file with placeholder parameter values.
#'
#' @details
#' Here, the rvp file is replaced with generic placeholder values to create a template file, which is commonly required for model calibration.
#' Although parameters may be found in other Raven input files, this command focuses on the rvp file. Other parameters
#' (such as gauge corrections in RVT or subbasin-level corrections in the RVH file) must be done manually.
#'
#' The Raven rvp file may be generated from the \code{\link{rvn_rvp_fill_template}} function.
#'
#' The list of parameters to be calibrated may be provided by the user (via \code{params_calibration} argument), or determined by RavenR. The intent of this
#' function is to provide a functional example of an RVP template file that may be used in calibration,
#' not a high quality calibration with clever selection of parameters to use. It is highly recommended
#' to build from the RVP template file created using expert hydrologic modelling knowledge.
#'
#' If \code{rvp_outfile} is not provided, Raven will attempt to write to the file prefix of the provided template file with a .rvp.tpl extension.
#' If there is a conflict with an existing file and \code{overwrite==FALSE}, the function will automatically overwrite a file
#' with the suffix "_ravenr_generated.rvp.tpl".
#'
#' Similarly with \code{ost_outfile}, the parameter default/min/max values and other Ostrich inputs will be written
#' based on a default template file to the ostIn.txt (or other provided file name). If this is set to NULL, the file will
#' not be written.
#'
#' The default parameter values come from the RavenParameters.dat file included with RavenR in the extdata folder. The
#' user may provide their own file with updated values if preferred. Note that the database files held in the RavenR
#' package are unofficial copies of those in the official Raven SVN, and any discrepancies should defer to the Raven SVN versions.
#'
#' Any parameters not found in this file will be ignored and a warning provided.
#'
#' If you find parameters not found by this function, please open an ticket on Github (\url{https://github.com/rchlumsk/RavenR/issues}).
#'
#' @param rvp_file path to the model *.rvp file
#' @param rvp_outfile file path to rewritten rvp file
#' @param ost_outfile file path to Ostrich input file
#' @param params_calibration vector of parameters to include in the calibration
#' @param overwrite whether to overwrite an existing template file (default \code{FALSE})
#' @param RavenParamsFile path to RavenParameters.dat file (default path points to file included with RavenR installation)
#' @return \code{TRUE} if the function executed successfully
#'
#' @seealso \code{\link{rvn_rvi_getparams}} to get parameter ranges from rvi.
#'
#' @examples
#'
#' # write rvp from template file
#' rvp_tempfile <- tempfile(fileext=".rvp")
#' rvn_rvp_fill_template(rvi_file=system.file("extdata","Nith.rvi", package="RavenR"),
#'                       rvh_file=system.file("extdata","Nith.rvh", package="RavenR"),
#'                       rvp_template_file =system.file("extdata","nithmodel.rvp_temp.rvp",
#'                       package="RavenR"),
#'                       rvp_out=rvp_tempfile)
#'
#' # setup calibration rvp template and ostin file
#' ost_tempfile <- tempfile(fileext=".txt")
#' rvptpl_tempfile <- tempfile(fileext=".rvp.tpl")
#' rvn_rvp_calib_template(rvp_file=rvp_tempfile,
#'                        rvp_outfile=rvptpl_tempfile,
#'                        ost_outfile=ost_tempfile)
#'
#' @importFrom dplyr left_join
#' @export rvn_rvp_calib_template
rvn_rvp_calib_template <- function(rvp_file=NULL, rvp_outfile=NULL, ost_outfile="ostIn.txt",
                                   params_calibration=NULL,
                                   overwrite=FALSE,
                                  RavenParamsFile=system.file("extdata","RavenParameters.dat", package="RavenR")) {

  ## check provided file inputs
  if (!is.character(rvp_file) & file.exists(rvp_file)) {
    stop("rvp_file must be a valid file path")
  }

  if (rvn_substrRight(rvp_file, 4) != ".rvp") {
    stop("rvp_file must be a valid Raven rvp file (*.rvp)")
  }

  ## read in rvp file
  tt <- readLines(rvp_file)

  ## set rvp_outfile
  if (is.null(rvp_outfile)) {
    rvp_outfile <- sprintf("%s.tpl",rvp_file)
  }

  ## check if rvp_outfile exists and overwrite is not enabled
  if (file.exists(rvp_outfile) & !overwrite) {
    rvp_outfile <- sprintf("%s_ravenr_generated.rvp.tpl",rvn_substrMRight(rvp_file,4))
    overwrite <- TRUE ## overwrite RavenR file is present
  }

  ## read in required .dat tables
  if (!file.exists(RavenParamsFile)) {
    stop(sprintf("Provided RavenParamsFile does not exist, please check: %s", RavenParamsFile))
  }
  cnames <- c("param","class_type","units","auto","default","min","max")
  RavenParamsTable<-read.table(RavenParamsFile,
                               sep="",
                               col.names=cnames,
                               header=FALSE,
                               blank.lines.skip=TRUE,
                               strip.white=TRUE,
                               stringsAsFactors=FALSE,
                               flush=TRUE,
                               comment.char = "#")

  ## get parameters to include
  if (is.null(params_calibration)) {
    params_calibration <- c("FOREST_COV",
                          "SNOW_SWI_MIN",
                          "SNOW_SWI_MAX",
                          "SNOW_SWI",
                          "RAINSNOW_TEMP",
                          "RAINSNOW_DELTA",
                          "TOC_MULTIPLIER",
                          "BASEFLOW_COEFF",
                          "PET_CORRECTION",
                          "MIN_MELT_FACTOR",
                          "MAX_MELT_FACTOR",
                          "DD_MELT_TEMP",
                          "HMETS_RUNOFF_COEFF",
                          "GAMMA_SHAPE",
                          "GAMMA_SCALE",
                          "GAMMA_SHAPE2",
                          "GAMMA_SCALE2",
                          "RAIN_ICEPT_PCT",
                          "SNOW_ICEPT_PCT")
  }

  # check for pairs of parameters that should be tied
  if ("SNOW_SWI_MIN" %in% params_calibration & "SNOW_SWI_MAX" %in% params_calibration) {
    warning("SNOW_SWI_MIN and SNOW_SWI_MAX parameters should be tied to ensure MIN <= MAX")
  }
  if ("MIN_MELT_FACTOR" %in% params_calibration & "MAX_MELT_FACTOR" %in% params_calibration) {
    warning("MIN_MELT_FACTOR and MAX_MELT_FACTOR parameters should be tied to ensure MIN <= MAX")
  }

  #### start writing file ----
  fc <- file(rvp_outfile, open="w+")
  # on.exit(close(fc))

  # track parameters
  kk <- 1
  param_notes <- c()
  param_type <- c()

  #### write header ----
  writeLines(sprintf("# Modified by RavenR::rvn_rvp_calib_template (%s)",Sys.Date()),fc)

  i <- 1
  while (i <= length(tt)) {
    temp <- unlist(strsplit(trimws(tt[i]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

    # take only temp items before a comment char
    if (any(rvn_substrLeft(temp,1) == "#")) {
      if (which(rvn_substrLeft(temp,1) == "#")[1] > 1) {
        temp <- temp[1:(which(rvn_substrLeft(temp,1) == "#")[1]-1)]
      } else {
        temp <- NULL
      }
    }

    if (is.null(temp) | length(temp)==0) {
      # nothing, can write as is
      writeLines(tt[i],fc)
      i <- i+1

    } else {

      ## generic for most blocks
      if (temp[1] %in% c(":LandUseClasses",":VegetationClasses",":SoilParameterList",
                         ":LandUseParameterList",":VegetationParameterList")) {

        start <- grep(pattern=sprintf(":%s",rvn_substrMLeft(temp[1],1)),tt)
        end <- grep(pattern=sprintf(":End%s",rvn_substrMLeft(temp[1],1)),tt)
        atts <- max(grep(pattern=":Attributes",tt[start:end])+start-1,
                    grep(pattern=":Parameters",tt[start:end])+start-1)
        units <- grep(pattern=":Units",tt[start:end])+start-1

        # read atts
        rvp_atts <- unlist(strsplit(trimws(tt[atts]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))[-1]

        if (any(params_calibration %in% rvp_atts)) {
          writeLines(tt[start],fc)
          writeLines(tt[atts],fc)
          writeLines(tt[units],fc)

          for (ii in (units+1):(end-1)) {
            temp <- unlist(strsplit(trimws(tt[ii]),split="\\,|\\s+|\\,\\s+|\\s+\\,\\s+"))

            temp_write <- temp[1]
            temp <- temp[-1]

            for (jj in 1:length(temp)) {
              if (rvp_atts[jj] %in% params_calibration) {
                temp_write <- c(temp_write,
                                sprintf("par_x%03d",kk))
                param_type <- c(param_type, rvp_atts[jj])
                param_notes <- c(param_notes,
                                 sprintf(":LandUseClasses %s %s",temp_write[1], rvp_atts[jj]))
                kk <- kk+1
              } else {
                temp_write <- c(temp_write,temp[jj])
              }
            }
            writeLines(sprintf("\t%s",paste0(temp_write, collapse=",\t")),fc)
          }
          writeLines(tt[end],fc)
          i <- end+1
        } else {
          # just write the block as per normal
          writeLines(tt[start:end],fc)
          i <- end+1
        }
      }

      ## GlobalParameter ----
      else if (temp[1] == ":GlobalParameter") {
        if (temp[2] %in% params_calibration) {
          writeLines(sprintf(":GlobalParameter  %s  par_x%03d",
                             temp[2],kk),fc)
          param_type <- c(param_type, temp[2])
                param_notes <- c(param_notes,
                                 sprintf(":GlobalParameter %s",temp[2]))
          kk <- kk+1
          i <- i+1
        } else {
          writeLines(tt[i],fc)
          i <- i+1
        }
      } else {
        writeLines(tt[i],fc)
        i <- i+1
      }
    }
  }
  writeLines("\n",fc)

  ##### close out file ----
  close(fc)

  ## prepare calib ranges table ----
  if (!is.null(ost_outfile)) {
    if (any(param_type %notin% RavenParamsTable$param)) {
      warning("Not all parameters found in RavenParamsTable, only those found will have ranges")
    }

    param_tbl <- left_join(data.frame("param"=param_type), RavenParamsTable[,c(1,5:7)], by="param") # xxx grab default,min,max, leave rest
    param_tbl$param_name <- sprintf("par_x%03d", seq(1,nrow(param_tbl)))

    # write ostIn.txt file based on template
    ostin_base <- system.file("extdata","ostIn_template.txt", package="RavenR")
    tt <- readLines(ostin_base)
    replace_index <- grep(pattern="  # add parameters here", x=tt)
    param_lines <- sprintf("  %s\t%s\t%s\t%s\tnone\tnone\tnone\t  # %s",
                           param_tbl$param_name,
                           param_tbl$default,
                           param_tbl$min,
                           param_tbl$max,
                           param_notes)

    # write new ostIn.txt file
    fc2 <- file(ost_outfile, open="w+")
    # on.exit(close(fc2))
    writeLines(tt[1:(replace_index-1)], fc2)
    writeLines(param_lines, fc2)
    writeLines(tt[(replace_index+1):length(tt)], fc2)
    writeLines("\n",fc2)
    close(fc2)
    warning("Additional updates required to ostIn.txt, including renaming file pairs, checking diagnostics, etc.")
  }

  return(TRUE)
}
