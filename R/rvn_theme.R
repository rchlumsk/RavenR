#' Raven R ggplot theme
#'
#' rvn_annual_peak makes the general Raven R Theme for all ggplots
#'
#' This function sets up the default theme for all ggplots generated using a
#' built in Raven R function. Made by adjusting the built in theme_bw()
#'
#'@export theme_RavenR

library(ggplot2)

theme_RavenR <- function(){
  theme_bw() +
    theme(panel.border = element_rect(color = "black"),
          legend.title = element_blank())
}

