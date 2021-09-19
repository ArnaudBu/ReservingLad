#' Cook's distance
#'
#' \code{Cook} plots Cook's distance for each age to age factor relatively to the linear model they belong.
#'
#' @param triangle Cumulated triangle as a matrix
#' @return a data.frame with Cook's distances as well as a plot displaying them
#'
#' @examples Cook(triangleExampleEngland)
#'
#' @export
Cook <- function(triangle){
  
  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}

  # Computations
  n <- ncol(triangle)
  dataA <- data.frame()
  for(i in 2:(n-1)){
    temp <- data.frame(x = triangle[, i-1], y = triangle[,i], devYear = colnames(triangle)[i], accidentYear = 1:nrow(triangle))
    temp <- temp[!(is.na(temp$y) | is.na(temp$x)),]
    fit <- lm(y~x+0, data=temp, weights = 1/temp$x)
    temp$cutoff <- 4/nrow(temp)
    temp$cookDistance = cooks.distance(fit)
    dataA <- rbind(dataA, temp)
  }
  dataA$devYearS <- factor(dataA$devYear, labels = paste0("dev period ", as.numeric(as.character(unique(dataA$devYear))) -1 , " to ", unique(dataA$devYear) ))

  # Plot
  a <- ggplot2::ggplot(data = dataA, ggplot2::aes(x = accidentYear, y = cookDistance)) +
    ggplot2::facet_wrap(~devYearS, scales = "free") +
    ggplot2::geom_bar(stat = "identity", color = "#FFB6C1", fill = 'lightgrey') +
    ggplot2::geom_segment(ggplot2::aes(x=accidentYear-0.5, xend=accidentYear+0.5, y=cutoff, yend=cutoff), color = "#FFB6C1", linetype = "dashed")+
    ggplot2::scale_x_continuous(breaks= 1:(nrow(triangle)-1)) +
    ggplot2::theme_minimal()+
    ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 0, hjust = 0))+
    ggplot2::xlab("")+
    ggplot2::ylab("")
  
  print(a)
  
  return(dataA)

}
