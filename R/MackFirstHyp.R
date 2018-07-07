#' Verify first Mack hypothesis
#'
#' \code{MackFirstHyp} verify first Mack hypothesis about the linearity of claims.
#'
#' @param triangle Undevelopped triangle as a matrix
#' @param mode Character. Plotting mode:
#' \itemize{
#'   \item{separate: Separate the plots by development period}
#'   \item{allinone: Keep all lines in one graph}}
#' @return a plot to confirm the hypothesis
#'
#' @import ggplot2
#' @import viridis
#'
#' @examples MackFirstHyp(triangleExampleEngland)
#'
#' @export
MackFirstHyp <- function(triangle, mode = "separate"){
  x <- y <- label <- devYear <- NULL
  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}

  if(mode == "separate"){

    # Construction of the table
    n <- ncol(triangle)
    dataPlot <- data.frame()
    for(i in 2:(n-1)){
      temp <- data.frame(x = triangle[, i-1], y = triangle[,i], devYear = colnames(triangle)[i], label = rownames(triangle))
      fit <- lm(y~x+0, data=temp)
      temp$yfit = predict(fit, temp)
      dataPlot <- rbind(dataPlot, temp)
    }
    dataPlot <- dataPlot[!is.na(dataPlot$y),]
    dataPlot$devYear <- factor(dataPlot$devYear, labels = paste0("dev year ", as.numeric(as.character(unique(dataPlot$devYear))) -1 , " to ", unique(dataPlot$devYear) ))
    #dataPlot <- rbind(dataPlot, data.frame(devYear = unique(dataPlot$devYear), x = 0, yfit = 0, label = NA, y = NA))

    # Plot of the table
    ggplot(data = dataPlot, aes(x = x, y = y, group = devYear, color = devYear)) +
      geom_point() +
      geom_line(aes(y = yfit)) +
      scale_x_continuous()+
      scale_y_continuous()+
      theme_minimal()+
      facet_wrap(~devYear, scales = "free") +
      scale_color_viridis(discrete = TRUE, "development \n year")+
      ylab("Paid amounts: development year N+1")+
      xlab("Paid amounts: development year N")+
      ggtitle("Proportionality between paid amounts: view N to N+1")+
      theme(strip.text.x = element_text(angle = 0, hjust = 0))+
      theme(legend.position = "none")

  } else {

  # Naming the columns
  if(is.null(rownames(triangle))){
    rownames(triangle) <- 1:nrow(triangle)
  }

  if(is.null(colnames(triangle))){
    colnames(triangle) <- 1:ncol(triangle)
  }

  # Construction of the table
  n <- ncol(triangle)
  dataPlot <- data.frame()
  for(i in 2:(n-1)){
    temp <- data.frame(x = triangle[, i-1], y = triangle[,i], devYear = colnames(triangle)[i], label = rownames(triangle))
    dataPlot <- rbind(dataPlot, temp)
  }
  dataPlot <- dataPlot[!is.na(dataPlot$y),]

  # Plot of the table
  ggplot(data = dataPlot, aes(x = x, y = y, group = devYear, color = devYear)) +
    #geom_point() +
    geom_line() +
    geom_label(aes(label = label), size = 3)+
    theme_minimal()+
    scale_color_viridis(discrete = TRUE)+
    ylab("Claims for development Year \n (Labels correspond to accident year)")+
    xlab("Claims for previous Year")

  }

}
