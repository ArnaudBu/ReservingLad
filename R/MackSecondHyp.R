#' Verify second Mack hypothesis
#'
#' \code{MackSecondHyp} verifies second Mack hypothesis about the independance of residuals.
#'
#' @param triangle Cumulated triangle as a matrix
#' @param weight Boolean matrix the same size of the triangle to tell if the value is to be considered: 1 for yes, 0 for no. First column is not considered
#' @return a plot to confirm the hypothesis
#'
#' @examples MackSecondHyp(triangleExampleEngland)
#'
#' @export
MackSecondHyp <- function(triangle, weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}

  # Naming the columns
  if(is.null(rownames(triangle))){
    rownames(triangle) <- 1:nrow(triangle)
  }

  if(is.null(colnames(triangle))){
    colnames(triangle) <- 1:ncol(triangle)
  }


  # Application of Chain Ladder
  outputCL <- ChainLadder(triangle, weight)

  # Construction of the table
  n <- ncol(triangle)
  dataPlot <- data.frame()
  for(i in 2:(n-1)){
    res <- (triangle[,i] - triangle[,i-1] * outputCL$lambda[i-1]) / sqrt(triangle[,i])
    temp <- data.frame(x = triangle[, i-1], y =  res / sqrt(mean(res^2, na.rm = TRUE)), devYear = colnames(triangle)[i], label = rownames(triangle))
    dataPlot <- rbind(dataPlot, temp)
  }
  dataPlot <- dataPlot[!is.na(dataPlot$y),]

  # Plot of the table
  a <- ggplot2::ggplot(data = dataPlot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_smooth(color = "#FFB6C1", method = 'loess', formula = y~x) +
    ggplot2::geom_label(ggplot2::aes(label = label, group = devYear, color = devYear), size = 3)+
    ggplot2::theme_minimal()+
    viridis::scale_color_viridis(discrete = TRUE)+
    ggplot2::ylab("Residuals for development year \n (Labels correspond to accident year)")+
    xlab("Claims for previous Year")
  
  print(a)
  return(dataPlot)
}
