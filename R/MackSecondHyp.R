#' Verify second Mack hypothesis
#'
#' \code{MackSecondHyp} verifies second Mack hypothesis about the independance of residuals.
#'
#' @param triangle Undevelopped triangle as a matrix
#' @return a plot to confirm the hypothesis
#'
#' @import ggplot2
#' @import viridis
#'
#' @examples MackFirstHyp(triangleExampleEngland)
#'
#' @export
MackSecondHyp <- function(triangle){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}

  # Naming the columns
  if(is.null(rownames(triangle))){
    rownames(triangle) <- 1:nrow(triangle)
  }
  
  if(is.null(colnames(triangle))){
    colnames(triangle) <- 1:ncol(triangle)
  }
  
  
  # Application of Chain Ladder
  outputCL <- ChainLadder(triangle)

  # Construction of the table
  n <- ncol(triangle)
  dataPlot <- data.frame()
  for(i in 2:(n-1)){
    temp <- data.frame(x = triangle[, i-1], y = (triangle[,i] - triangle[,i-1] * outputCL$lambda[i-1]) / sqrt(triangle[,i]), devYear = colnames(triangle)[i], label = rownames(triangle))
    dataPlot <- rbind(dataPlot, temp)
  }
  dataPlot <- dataPlot[!is.na(dataPlot$y),]

  # Plot of the table
  ggplot(data = dataPlot, aes(x = x, y = y)) +
    #geom_point() +
    #geom_line() +
    geom_smooth(color = rgb(99/255, 24/255, 66/255)) +
    geom_label(aes(label = label, group = devYear, color = devYear), size = 3)+
    theme_minimal()+
    scale_color_viridis(discrete = TRUE)+
    ylab("Residuals for development year \n (Labels correspond to accident year)")+
    xlab("Claims for previous Year")
}
