#' @title RavenR ggplot theme
#'
#' @description
#' rvn_theme_RavenR makes the general Raven R Theme for all ggplots
#'
#' @details
#' This function sets up the default theme for all ggplots generated using a
#' built in Raven R function. Made by adjusting the built in theme_bw().
#'
#' @return returns a theme for usage in ggplot2 figures
#'
#' @seealso \code{\link{rvn_annual_volume}} to create a scatterplot of annual flow
#' volumes.
#'
#' See also \href{http://www.civil.uwaterloo.ca/jrcraig/}{James R.
#' Craig's research page} for software downloads, including the
#' \href{http://www.civil.uwaterloo.ca/jrcraig/Raven/Main.html}{Raven page}
#'
#' @examples
#' # generate a basic ggplot and apply the RavenR theme
#' ggplot(data=cars, aes(x=speed, y=dist))+
#' geom_point()+
#' rvn_theme_RavenR()
#'
#' @keywords RavenR theme ggplot plot figure aesthetics
#'
#' @export rvn_theme_RavenR
#' @importFrom ggplot2 theme theme_bw element_rect element_blank
rvn_theme_RavenR <- function(){
  theme_bw() +
    theme(panel.border = element_rect(color = "black"),
          legend.title = element_blank())
    # theme(axis.title = element_text(face = 'bold'))
}