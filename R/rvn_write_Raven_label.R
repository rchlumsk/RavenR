#' @title Writes common Raven labeled line to file, with optional value (appends)
#'
#' @param label character, (e.g. "SoilClasses")
#' @param filename character, file name/path to write to, with extension
#' @param value numeric or character, corresponding value written after label (optional)
#' @param digits Number of digits to round value to (optional)
#'
#' @return TRUE returns TRUE if executed successfully
#'
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#' @examples
#' rvn_write_Raven_label('Duration', 'Hogwarts.rvi', value=365)
#'
#' # cleanup temporary file
#' unlink("Hogwarts.rvi")
#'
#' @export rvn_write_Raven_label
rvn_write_Raven_label <- function(label, filename, value=NULL, digits=NULL)
{
  if (!is.null(digits)) {
    value <- round(value, digits)
  }
  write(paste0(':',label, ' ', value), filename, append = TRUE)
  return(TRUE)
}
