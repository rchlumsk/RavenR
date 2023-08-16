#' @title Update command in Raven input file
#'
#' @description
#' Updates the provided rvi file with the command and value provided.
#'
#' @param filename the name of the .rvi file (with .rvi extension included), either relative
#' to the working directory or absolute.
#' @param command the rvi command with preceeding colon to update
#' @param value value of the command to update
#' @param outputfile if writing to a new file, otherwise filename is overwritten
#'
#' @return {returns \code{TRUE} if run successfully}
#'
#' @author James R. Craig, University of Waterloo
#'
#' @seealso
#' \code{\link{rvn_rvi_read}} to read and process rvi files with RavenR
#'
#' @examples
#'   # load example rvi file
#'   ff <- system.file("extdata","Nith.rvi", package="RavenR")
#'   tf <- tempfile(fileext=".rvi")
#'
#'   rvn_rvi_commandupdate(filename=ff,
#'   command=":StartDate",
#'   value="2022-10-01 00:00:00",
#'   outputfile=tf)
#'
#' @export rvn_rvi_commandupdate
rvn_rvi_commandupdate <- function(filename=NULL,command=NULL,value=NULL, outputfile=NA)
{

  stopifnot(file.exists(filename))
  if (is.null(command)) {stop("rvn_update_command: command must be provided")}
  if (is.null(value)) {
    value <- " "
    warning("rvn_update_command: value not provided, will be left blank")
  }

  # update outputfile is not provided (overwrite same file)
  if (is.na(outputfile)) {
    outputfile <- filename
  }

  rvi<-readLines(filename)
  # rvinew <- rvi

  # check for command in file
  if (length(grep(pattern="",x=rvi))==0) {
    warning(sprintf("rvn_update_command: command %s not found in file %s, and will be added to file.",
                   command, filename))
    rvinew <- rvi
    rvinew[length(rvi)+1] <- sprintf("%s %s\n",command, value)

  } else {
    # check for
    rvinew<-gsub(paste0("^",command,".*$"),paste0(command," ",value), rvi)
  }

  writeLines(rvinew,con=outputfile)
  print(sprintf("Raven file %s has been updated for command %s with value %s",
                filename, command, value))
  return(TRUE)
}
