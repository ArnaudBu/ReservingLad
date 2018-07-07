#' Settlement rate
#'
#' \code{Settlement} plots the settlement rate for each development year.
#'
#' @param triangle Undevelopped triangle as a matrix
#' @return a plot
#'
#' @import ggplot2
#' @import viridis
#'
#' @examples Settlement(triangleExampleEngland)
#'
#' @export
Settlement <- function(triangle){

  # Preparing the data
  triangleComp <- ChainLadder(triangle, weight)$developedTriangle
  triangleComp <- triangleComp / matrix(rep(triangleComp[,ncol(triangleComp)], ncol(triangleComp)), ncol = ncol(triangleComp))
  triangleComp[is.na(triangle)] <- NA
  tcDat <- data.table(melt(triangleComp))
  tcDat <- tcDat[!is.na(value)]

  # Plot
  ggplot(tcDat[tcDat$Var1 != 7,], aes(x = Var2, y = value, group = factor(Var1), color = factor(Var1))) +
    geom_line(size = 1.3) +
    scale_color_viridis(discrete = TRUE, "underwriting \n year") +
    theme_minimal() +
    scale_y_continuous()+
    ylab("") +
    xlab("") +
    ggtitle("Settlement pattern by development year")
}
