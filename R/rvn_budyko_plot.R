#' @title Budkyo Plot
#'
#' @description
#' rvn_budyko_plot creates a Budyko plot, adding supplied data points if provided.
#'
#' @details
#' Creates a blank Budyko curve plot if no data is provided. Labels may optionally
#' be added to the plot with \code{limiting_labels=TRUE} to indicate where in the
#' curve the energy-limited and water-limited limits are. The original Budyko curve
#' may also be added with \code{budyko_curve=TRUE}.
#'
#' Data may be provided and plotted in the graph as well. If data is provided, it can be provided as:
#' - x: an xts object with PRECIP, AET, and PET columns
#' - x_indices: an xts object with indices calculated for each year, columns named ARIDITY and EVAPORATIVE
#'
#' @references Budyko, M. I. (1974), Climate and Life, Academic Press, New York.
#'
#' @param x extensible time series object of PET, AET, and PRECIP (optional)
#' @param x_indices extensible time series object of annual ARIDITY and EVAPORATION indices (optional)
#' @param limiting_labels boolean whether to vertical line at x=1 and labels for 'Energy Limited' and 'Water Limited' to plot
#' @param budyko_curve boolean whether to add curve to plot
#' @param mm month of water year ending (default 9)
#' @param dd day of water year ending (default 30)
#' @return \item{p1}{returns Budyko plot as ggplot object}
#'
#' @seealso \code{\link{rvn_watershedmeb_read}} for reading in the WatershedMassEnergyBalance.csv file, and
#' \code{\link{rvn_apply_wyearly}} to apply functions over the water year.
#'
#' @examples
#'
#' # return blank Budyko plot
#' rvn_budyko_plot()
#'
#' # return blank plot with labels and curve added
#' rvn_budyko_plot(limiting_labels=TRUE, budyko_curve=TRUE)
#'
#' # plot sample data on Budyko plot (two years of data)
#' wstor <- system.file("extdata","run1_WatershedStorage.csv", package="RavenR") %>%
#'            rvn_watershed_read()
#' ff <- system.file("extdata","run1_ForcingFunctions.csv", package="RavenR") %>%
#'            rvn_forcings_read()
#'
#' library(xts)
#' precip <- ff$forcings$rain+ff$forcings$snow
#' pet <- ff$forcings$PET
#' aet <- diff.xts(x=wstor$watershed_storage$Cum..Losses.to.Atmosphere..mm.,
#'           k=1, na.pad=TRUE)
#' aet[1] <- 0
#'
#' x <- merge.xts(precip,pet,aet)
#' names(x) <- c("precip","pet","aet")
#' rvn_budyko_plot(x=x, budyko_curve=TRUE)
#'
#' @export rvn_budyko_plot
#' @importFrom ggplot2 fortify ggplot geom_line scale_x_date xlab ylab theme aes scale_colour_brewer geom_bar coord_cartesian geom_vline
#' @importFrom lubridate year
rvn_budyko_plot <- function(x=NULL, x_indices=NULL, limiting_labels=FALSE, budyko_curve=FALSE,
                         mm=9, dd=30)
{

  y <- ARIDITY <- EVAPORATIVE <- PET.P <- AET.P <- NULL

  if (!is.null(x)) {
    if ("xts" %notin% class(x)) {
      stop("rvn_budyko_plot: x should be of class xts")
    }
    names(x) <- toupper(names(x))
    if (any(c("PET","AET","PRECIP") %notin% names(x)) ) {
      stop(sprintf("rvn_budyko_plot: x should contain columns for PET, AET, and PRECIP;\nNissing columns for %s",
                   c("PET","AET","PRECIP")[which(c("PET","AET","PRECIP") %notin% names(x))] ))
    }
  }

  if (!is.null(x_indices)) {
    if ("xts" %notin% class(x_indices)) {
      stop("rvn_budyko_plot: x_indices should be of class xts")
    }
    names(x_indices) <- toupper(names(x_indices))
    if (c("PET","AET","PRECIP") %notin% names(x_indices)) {
      stop(sprintf("rvn_budyko_plot: x_indices should contain columns for ARIDITY and EVAPORATIVE;\nNissing columns for %s",
                   c("ARIDITY","EVAPORATIVE")[which(c("ARIDITY","EVAPORATIVE") %notin% names(x_indices))] ))
    }
  }

  if (!is.null(x) & !is.null(x_indices)) {
    warning("rvn_budyko_plot: both indices and raw data supplied with both x_indices and x; only the supplied indices will be used.")
    x <- NULL
  }

  # base Budyko curve
  p1 <- ggplot(data=data.frame(PET.P=seq(0,20,1),AET.P=c(0,rep(1,20))),aes(x=PET.P,y=AET.P))+
    geom_line()+xlab("Aridity Index (PET/P)")+ylab("Evaporative Index (AET/P)")+
    coord_cartesian(xlim=c(0,2))+
    rvn_theme_RavenR()

  if (limiting_labels) {
    p1 <- p1 +
      geom_vline(xintercept=1,lty=3)+
      geom_text(data=data.frame(
        labels=c("Energy Limited","Water Limited"),
        x=c(0.7,1.3),y=rep(0.1,2)),
        aes(x=x,y=y,label=labels))
  }

  if (budyko_curve) {
    xx <- seq(0,20,length.out=1000)
    f_budyko <- function(x) (x*tanh(1/x)*(1-exp(-x)))^0.5
    yy <- f_budyko(xx)

    p1 <- p1 +
      geom_line(data=data.frame(xx,yy), aes(x=xx, y=yy),
                linetype=2)
  }

  if (!is.null(x)) {
    # convert x into x_indices
    temp <- rvn_apply_wyearly(x, FUN=mean,na.rm=TRUE,mm=mm,dd=dd)

    if (year(temp[nrow(temp)]) == year(temp[(nrow(temp)-1)])) {
      temp <- temp[1:(nrow(temp)-1),]
    }

    temp$ARIDITY <- temp$PET / temp$PRECIP
    temp$EVAPORATIVE <- temp$AET / temp$PRECIP

    x_indices <- rvn_fortify_xts(temp)[,c("Date","ARIDITY","EVAPORATIVE")]
  }

  if (!is.null(x_indices)) {

    # convert xts to data frame if required
    if ("xts" %in% x_indices) {
      x_indices <- rvn_fortify_xts(x_indices)
    }

    # add to plot
    p1 <- p1 +
      geom_point(data=x_indices, aes(x=ARIDITY, y=EVAPORATIVE))
  }

  # add RavenR theme for ggplot plots (currently done earlier in plot creation)
  # p1 <- p1 + rvn_theme_RavenR()

  return(p1)
}

