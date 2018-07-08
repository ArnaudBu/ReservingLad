#' Cook's distance
#'
#' \code{Cook} plots Cook's distance for each age to age factor relatively to the linear model they belong.
#'
#' @param triangle Undevelopped triangle as a matrix
#' @return a plot to displaying the Cook's distances
#'
#' @import ggplot2
#' @import viridis
#'
#' @examples Cook(triangleExampleEngland)
#'
#' @export
Cook <- function(triangle){
  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}

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
  ggplot(data = dataA, aes(x = accidentYear, y = cookDistance)) +
    facet_wrap(~devYearS, scales = "free") +
    geom_bar(stat = "identity", color = rgb(99/255, 24/255, 66/255), fill = 'lightgrey') +
    geom_segment(aes(x=accidentYear-0.5, xend=accidentYear+0.5, y=cutoff, yend=cutoff), color = rgb(99/255, 24/255, 66/255), linetype = "dashed")+
    scale_x_continuous(breaks= 1:(nrow(triangle)-1)) +
    theme_minimal()+
    theme(strip.text.x = element_text(angle = 0, hjust = 0))+
    xlab("")+
    ylab("")

}
