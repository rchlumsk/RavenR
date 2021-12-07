#' @title Write common Raven file header
#'
#' @description
#' Writes the common Raven file header to file. All lines are Appended.
#'
#' @param filename Name of the file, with extension
#' @param filetype File extension, Encoding, Raven version (e.g. "rvp ASCII Raven 2.9.1")
#' @param author Name of file author (optional)
#' @param creationDate Bool of whether creation date should be added to header. (default TRUE)
#' @param textlen Length of lines (default: 40, used to right-align text)
#'
#' @return TRUE returns TRUE if executed successfully
#'
#' @author Leland Scantlebury, \email{leland@@scantle.com}
#'
#' @examples
#' tf <- file.path(tempdir(), 'HogwartsBasin.rvp')
#' rvn_write_Raven_header(filename = tf,
#'                     filetype = 'rvp ASCII Raven 2.9.1',
#'                     author   = 'Harry Potter')
#'
#' # view file
#' readLines(tf)
#'
#' @export rvn_write_Raven_header
rvn_write_Raven_header <- function(filename, filetype, author=NULL,
                                creationDate=TRUE, textlen=40)
{
  # File Type
  write(paste0(":FileType ", rvn_stringpad(filetype, textlen-10)),
        append=TRUE, file=filename)

  # Author
  if (!is.null(author)) {
    write(paste0(":WrittenBy ", rvn_stringpad(author, textlen-11)),
          append=TRUE, file=filename)
  }

  # Creation Date
  if (creationDate) {
    dateString <- format(Sys.Date(), '%b %Y')
    write(paste0(":CreationDate ", rvn_stringpad(dateString, textlen-14)),
          append=TRUE, file=filename)
  }

  return(TRUE)
}
