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

  # Checks
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}

  # Preparing the data
  triangleComp <- ChainLadder(triangle)$developedTriangle
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
