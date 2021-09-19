#' Verify first Mack hypothesis
#'
#' \code{MackFirstHyp} verifies first Mack hypothesis about the linearity of claims.
#'
#' @param triangle Cumulated triangle as a matrix
#' @param mode Character. Plotting mode:
#' \itemize{
#'   \item{separate: Separate the plots by development period}
#'   \item{allinone: Keep all lines in one graph}}
#' @return a data.frame with the data, along with a plot to confirm the hypothesis
#'
#'
#' @examples MackFirstHyp(triangleExampleEngland)
#'
#' @export
MackFirstHyp <- function(triangle, mode = "separate"){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}

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
    dataPlot$devYear <- factor(dataPlot$devYear, labels = paste0("dev period ", as.numeric(as.character(unique(dataPlot$devYear))) -1 , " to ", unique(dataPlot$devYear) ))

    # Plot of the table
    a <- ggplot2::ggplot(data = dataPlot, ggplot2::aes(x = x, y = y, group = devYear, color = devYear)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(y = yfit)) +
      ggplot2::scale_x_continuous()+
      ggplot2::scale_y_continuous()+
      ggplot2::theme_minimal()+
      ggplot2::facet_wrap(~devYear, scales = "free") +
      viridis::scale_color_viridis(discrete = TRUE, "development \n year")+
      ggplot2::ylab("Paid amounts: development year N+1")+
      ggplot2::xlab("Paid amounts: development year N")+
      ggplot2::ggtitle("Proportionality between paid amounts: view N to N+1")+
      ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 0, hjust = 0))+
      ggplot2::theme(legend.position = "none")
    
    print(a)
    return(dataPlot)
    
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
  a <- ggplot2::ggplot(data = dataPlot, ggplot2::aes(x = x, y = y, group = devYear, color = devYear)) +
    ggplot2::geom_line() +
    ggplot2::geom_label(ggplot2::aes(label = label), size = 3)+
    ggplot2::theme_minimal()+
    viridis::scale_color_viridis(discrete = TRUE)+
    ggplot2::ylab("Claims for development Year \n (Labels correspond to accident year)")+
    ggplot2::xlab("Claims for previous Year")
  
  print(a)
  return(dataPlot)

  }

}
