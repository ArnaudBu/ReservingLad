#' Chain Ladder
#'
#' \code{ChainLadder} applies the Chain Ladder method to a cumulated claim triangle.
#'
#' @param triangle Cumulated triangle as a matrix
#' @param weight Boolean matrix the same size of the triangle to tell if the value is to be considered: 1 for yes, 0 for no. First column is not considered
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangle: the input triangle }
#'   \item{developedTriangle: the developed triangle}
#'   \item{lambdas: the passing coefficients}
#'   \item{ibnrByAccidentYear: ibnr reserve by accident year}
#'   \item{ibnr: total ibnr reserve}}
#'
#' @details Missing values are handled. They just need to be replaced by a NA.
#'
#' @examples outputCL <- ChainLadder(triangleExampleEngland)
#'
#' @export
ChainLadder <- function(triangle, weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(is.na(weight[1])){weight = !is.na(triangle)}
  if(!(nrow(weight) == nrow(triangle) & ncol(weight) == ncol(triangle))){stop("Invalid weight dimension")}
  if(!(sum((weight == 0) + (weight == 1), na.rm = TRUE) == sum(!is.na(weight)))){stop("Invalid values in weights")}

  # Handle the case when weight is 0 on the first row last column
  
  if(weight[1, ncol(triangle)] == 0){
    triangle[1, ncol(triangle)] <- triangle[1, ncol(triangle)-1]
    weight[1, ncol(triangle)] <- 1
  }
  
  # Compute the passing coefficients
  n <- ncol(triangle)
  lambda <- sapply(1:(n-1),
                   function(i){
                     sum(triangle[,i+1] * weight[, i+1], na.rm = T) /
                       sum(triangle[,i] * weight[, i+1], na.rm = T)})
  names(lambda) <- colnames(triangle)[2:n]

  # Verify the validity of lambda
  if(!all(!is.na(lambda))){stop("Unable to compute passing coefficients.")}

  # Apply the coefficients to create the developed triangle
  developedTriangle <- triangle
  for(i in 2:n){
    developedTriangle[is.na(developedTriangle[,i]),i] <- developedTriangle[is.na(developedTriangle[,i]),i-1] * lambda[i-1]
  }

  # Calculate IBNR by accident year
  ibnr <- developedTriangle[,n] - ReservingLad::latest(triangle)

  # Return the values
  return(list(triangle = triangle,
              developedTriangle = developedTriangle,
              lambdas = lambda,
              ibnrByAccidentYear = ibnr,
              ibnr = sum(ibnr)
  ))
}
