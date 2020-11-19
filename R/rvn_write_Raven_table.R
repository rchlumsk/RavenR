#' @title Writes a nicely formatted tables of Raven attributes/parameters
#'
#' @param attributes array of strings containing attribute/parameter names
#' @param units array of strings with the corresponding units
#' @param df Dataframe of values corresponding to attributes/parameters
#' @param filename Name of the file, with extension, to append the table to
#' @param id_col True/False of whether an numeric id column is the first column in the table
#' and, in common Raven fashion, does not have a corresponding attribute (default: True)
#' @param justify alignment of character columns (default 'right'). See \code{\link{format}}
#' @param sep character(s) used to seperate columns (default ', ')
#' @param ... Extra arguments for \code{\link{write.fwf}}
#'
#' @return TRUE returns TRUE if executed successfully
#'
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#'
#' @examples
#' soil_classes <- data.frame('Attributes' = c('DEFAULT','ALTERNATIVE'),
#'                            'SAND'      = c(0.4316, 0.3000),
#'                            'CLAY'      = c(0.1684, 0.4000),
#'                            'SILT'      = c(0.4000, 0.3000),
#'                            'ORGANIC'   = c(0.0000, 0.0000))
#' attributes <- c('%SAND','%CLAY','%SILT','%ORGANIC')
#' units <-  rep('none',4)
#'
#' tf <- file.path(tempdir(), "Hogwarts.rvp")
#' rvn_write_Raven_table(tf, attributes = attributes, units = units, df = soil_classes)
#'
#' # view file
#' readLines(tf)
#'
#' @export rvn_write_Raven_table
#' @importFrom gdata write.fwf
rvn_write_Raven_table <- function(attributes, units, df, filename, id_col=TRUE,
                             justify = 'right', sep = ', ', ...)
{

  # Setup
  towrite <- rvn_df_to_Raven_table(attributes, units, df, id_col)

  # Write using write.fwf from gdata
  write.fwf(x = towrite,
            file = filename,
            append = TRUE,
            justify = justify,
            colnames = FALSE,
            sep = sep,
            scientific = TRUE,
            ...)

  return(TRUE)
}
