#' Settlement rate
#'
#' \code{Settlement} plots the settlement rate for each development year.
#'
#' @param triangle Cumulated triangle as a matrix
#' @return values and a plot to confirm the hypothesis
#'
#' @examples Settlement(triangleExampleEngland)
#'
#' @export
Settlement <- function(triangle){

  # Checks
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}

  # Preparing the data
  triangleComp <- ChainLadder(triangle)$developedTriangle
  triangleComp <- triangleComp / matrix(rep(triangleComp[,ncol(triangleComp)], ncol(triangleComp)), ncol = ncol(triangleComp))
  triangleComp[is.na(triangle)] <- NA
  triangleCompDT <- data.table::data.table(triangleComp)
  triangleCompDT[, group := factor(row.names(triangle), levels = row.names(triangle))]
  tcDat <- data.table::melt(triangleCompDT, id.vars = c("group"))
  tcDat <- tcDat[!is.na(value)]

  # Plot
  a<- ggplot2::ggplot(tcDat, ggplot2::aes(x = variable, y = value, group = group, color = group)) +
    ggplot2::geom_line(size = 1.3) +
    viridis::scale_color_viridis(discrete = TRUE, "underwriting \n year") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous()+
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("Settlement pattern by development year")
  
  print(a)
  return(as.data.frame(tcDat))
}
