#' @title Sets up tables for writing to Raven input files
#'
#' @param attributes array of strings containing attribute/parameter names
#' @param units array of strings with the corresponding units
#' @param df Dataframe of values corresponding to attributes/parameters
#' @param id_col True/False of whether an numeric id column is the first column in the table
#' and, in common Raven fashion, does not have a corresponding attribute (default: True)
#' @param parameters bool, when adding attribues/parameter tag, should ':Parameters' be used instead of ':Attributes'?
#'
#' @return outdf data.frame object
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#' @examples
#' soil_classes <- data.frame('Attributes' = c('DEFAULT','ALTERNATIVE'),
#'                            'SAND'      = c(0.4316, 0.3000),
#'                            'CLAY'      = c(0.1684, 0.4000),
#'                            'SILT'      = c(0.4000, 0.3000),
#'                            'ORGANIC'   = c(0.0000, 0.0000))
#' attributes <- c('%SAND','%CLAY','%SILT','%ORGANIC')
#' units <-  rep('none',4)
#' soil_classes <- rvn_df_to_Raven_table(attributes, units, soil_classes)
#' print(soil_classes)
#'
#' @export rvn_df_to_Raven_table
rvn_df_to_Raven_table <- function(attributes, units, df, id_col=TRUE, parameters=FALSE)
{

  # Get new column names
  outnames <- paste0('col',1:length(names(df)))

  # Ensure attributes/parameters in Upper Case
  attributes <- toupper(attributes)

  # Add Attribute and Unit lines (check if necessary, is that even necessary?)
  # TODO: What if a list is passed?
  # TODO: Explicit handling for Attributes vs Parameters
  if (trimws(attributes[1]) != ':ATTRIBUTES' &
      trimws(attributes[1]) != ':PARAMETERS' &
      trimws(attributes[1]) != 'ATTRIBUTES' &
      trimws(attributes[1]) != 'PARAMETERS') {
    if (!parameters){ attributes <- c('  :Attributes', attributes) }
    else { attributes <- c('  :Parameters', attributes) }
  } else if (trimws(attributes[1]) == 'ATTRIBUTES' || trimws(attributes[1]) == ':ATTRIBUTES') {
    attributes[1] <- c('  :Attributes') # Custom spacing
  } else if (trimws(attributes[1]) == 'PARAMETERS' || trimws(attributes[1]) == ':PARAMETERS') {
    attributes[1] <- c('  :Parameters') # Custom spacing
  }

  # Unit line also has custom spacing for proper alignment
  if (toupper(trimws(units[1])) != ':UNITS' &
      toupper(trimws(units[1])) != 'UNITS'){
    units <-  c("  :Units     ", units)
  }else{
    units[1] <- "  :Units     "
  }

  # Correction for no ID col (doesn't add an extra comma)
  if (!id_col) {
    attributes <- c(paste(attributes[1], attributes[2], sep=', '),
                    attributes[3:length(attributes)])
    #-- spacing gets messed up if attribute[2] is longer than units[2]
    #   ...opposite could be true but is not currently corrected
    coldiff <- max(0, nchar(attributes[2]) - nchar(units[2]))
    unit2_spaced <- paste0(rep(' ',coldiff), units[2])
    units <- c(paste(units[1], unit2_spaced, sep=', '),
               units[3:length(units)])
  }

  # Break if we're not going to have the right number of things
  if (length(outnames) != length(attributes)) {
    stop('Number of attributes/parameters does not match number of dataframe columns')
  }
  if (length(outnames) != length(units)) {
    stop('Number of units does not match number of dataframe columns')
  }

  # Mix n' match
  outdf <- data.frame(rbind(attributes, units),
                      row.names = NULL, stringsAsFactors = FALSE)
  names(df) <- outnames
  names(outdf) <- outnames

  outdf <- rbind(outdf, df)

  return(outdf)
}
