#' @title Writes common Raven labeled line to file, with optional value (appends)
#'
#' @param label character, (e.g. "SoilClasses")
#' @param filename character, file name/path to write to, with extension
#' @param value numeric or character, corresponding value written after label (optional)
#' @param digits Number of digits to round value to (optional)
#' @param indent_level Adds two spaces before label for every one level (default = 0)
#'
#' @return TRUE returns TRUE if executed successfully
#'
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#' @examples
#'
#' tf <- file.path(tempdir(), "Hogwarts.rvi")
#'
#' # Numeric example
#' rvn_write_Raven_label('Duration', filename=tf, value=365)
#'
#' # Hydrologic Processes
#' rvn_write_Raven_label('HydrologicProcesses', tf)
#'
#' # String example, with indent
#' rvn_write_Raven_label('SnowBalance', filename = tf,
#'                       value = paste('SNOBAL_HMETS', 'MULTIPLE', 'MULTIPLE'),
#'                       indent_level = 1)
#'
#' # Preview file
#' readLines(tf)
#'
#' @export rvn_write_Raven_label
rvn_write_Raven_label <- function(label, filename, value=NULL, digits=NULL, indent_level=0)
{
  if (!is.null(digits)) {
    value <- round(value, digits)
  }
  write(paste0(rep('  ', indent_level), ':',label, ' ', value), filename, append = TRUE)
  return(TRUE)
}
