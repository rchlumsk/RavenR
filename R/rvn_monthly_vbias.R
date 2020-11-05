#' @title Monthly Volume Bias
#'
#' @description
#' rvn_monthly_vbias creates a plot of the monthly volume biases in the simulated
#' flow series.
#'
#' @details
#' This function calculates the monthly volume biases and optionally creates a
#' plot of them. The monthly volume biases are averaged across all years of
#' data. If normalized, the biases are calculated as:
#'
#' (Vi_sim - Vi_obs)/Vi_obs*100
#'
#' to be expressed as a percent error.
#'
#' The sim and obs should be of time series (xts) format and are assumed to be
#' of the same length and time period. The flow series are assumed to be daily
#' flows with units of m3/s. Note that a plot title is purposely omitted in
#' order to allow the automatic generation of plot titles.
#'
#' The add_labels will add the labels of 'overestimated' and 'underestimated'
#' to the right hand side axis if set to TRUE. This is useful in interpreting
#' the plots. Note that the biases are calculated as sim_Volume - obs_Volume,
#' which means that negative values mean the volume is underestimated, and
#' positive values mean the volume is overestimated.
#'
#' @param sim time series object of simulated flows
#' @param obs time series object of observed flows
#' @param add_line optionally adds a horizontal line to the plot for reference
#' (default TRUE)
#' @param normalize option to normalize the biases and report as percent error
#' (default TRUE)
#' @param add_labels optionally adds labels for early peak/late peaks on right
#' side axis (default TRUE)
#' @return \item{mvbias}{monthly volume biases}
#'
#' @seealso \code{\link{rvn_annual_volume}} to create a scatterplot of annual flow
#' volumes
#'
#' @examples
#' # load sample hydrograph data, two years worth of sim/obs
#' data(rvn_hydrograph_data)
#' sim <- rvn_hydrograph_data$hyd$Sub36
#' obs <- rvn_hydrograph_data$hyd$Sub36_obs
#'
#' # check the monthly volume bias; normalizes by default
#' rvn_monthly_vbias(sim,obs)
#'
#' # check unnormalzied monthly volume biases; see the larger volumes in certain periods
#' rvn_monthly_vbias(sim,obs,normalize = FALSE)
#'
#' @export rvn_monthly_vbias
#' @importFrom ggplot2 ggplot geom_bar aes scale_y_continuous scale_x_continuous geom_hline geom_text
#' @importFrom xts apply.monthly
rvn_monthly_vbias <- function (sim, obs, add_line = TRUE, normalize = TRUE, add_labels = TRUE)
{

  nmon <- NULL

  obs.monthly <- apply.monthly(obs, sum, na.rm = TRUE)
  sim.monthly <- apply.monthly(sim, sum, na.rm = TRUE)
  mvbias <- matrix(NA, nrow = 12, ncol = 1)
  colnames(mvbias) <- c("mvbias")
  rownames(mvbias) <- rvn_month_names(TRUE)
  if (normalize) {
    diff <- (sim.monthly - obs.monthly)/obs.monthly * 100
    y.lab <- "% Flow Volume Bias"
  }
  else {
    diff <- (sim.monthly - obs.monthly)
    y.lab <- "Flow Volume Bias (m3/s)"
  }
  for (k in 1:12) {
    mvbias[k] <- mean(diff[month(diff) == k, ], na.rm = TRUE)
  }

  df.plot <- data.frame(cbind(seq(1,12),rownames(mvbias),mvbias))
  colnames(df.plot) <-c("nmon","x.label","mvbias")
  df.plot$mvbias <- as.numeric(as.character(df.plot$mvbias))
  df.plot$nmon <- as.numeric(as.character(df.plot$nmon))

  limit <- ceiling(max(abs(mvbias), na.rm = TRUE))

  p1 <- ggplot(df.plot)+
    geom_bar(aes(x=nmon,y=mvbias),stat="identity",width=0.5)+
    scale_y_continuous(name=y.lab)+
    scale_x_continuous(breaks=df.plot$nmon,labels=df.plot$x.label,name="")+
    rvn_theme_RavenR()

  if (add_line) {
    p1 <- p1+
      geom_hline(yintercept=0,linetype=2)
  }
  if (add_labels) {
    if (max(mvbias, na.rm = TRUE)/2 > 0) {
      p1 <- p1+
        scale_y_continuous(name=y.lab, limits = c(-limit, limit)) +
        geom_text(x= max(as.numeric(df.plot$nmon)+0.5),
                  y= limit/2,
                  label= "Overestimated",
                  angle=90,
                  vjust = 0.5,
                  hjust = 0.5)
        }
    if (min(mvbias, na.rm = TRUE)/2 < 0) {
      p1 <- p1+
        geom_text(x= max(as.numeric(df.plot$nmon)+0.5),
                  y= -limit/2,
                  label= "Underestimated",
                  angle=90,
                  vjust = 0.5,
                  hjust = 0.5)
    }
  }
  return(list(df.mvbias = mvbias,plot=p1))
}
