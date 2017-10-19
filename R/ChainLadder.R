#' Chain Ladder
#'
#' \code{ChainLadder} applies the Chain Ladder method to a cumulated claim triangle.
#'
#' @param triangle Undevelopped triangle as a matrix
#' @param weight Boolean matrix with 1 row and 1 column less than the triangle to tell if the link ratio is to be considered: 1 for yes, 0 for no
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangle: the input triangle }
#'   \item{developedTriangle: the developed triangle}
#'   \item{lambdas: the passing coefficients}
#'   \item{ibnrByAccidentYear: ibnr reserve by accident year}
#'   \item{ibnr: total ibnr reserve}}
#'
#' @details Missing values are handled. There just need to be replaced by a NA.
#'
#' @examples outputCL <- ChainLadder(triangleExampleEngland)
#'
#' @export
ChainLadder <- function(triangle, weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}
  if(is.na(weight)){weight <- !is.na(triangle[-n, -1])}
  if(!(nrow(weight) == nrow(triangle) - 1 & ncol(weight) == ncol(triangle) -1)){stop("Invalid weight dimension")}
  if(!(sum((weight == 0) + (weight == 1), na.rm = TRUE) == sum(!is.na(weight)))){stop("Invalid values in weights")}

  # Compute the passing coefficients
  lambda <- sapply(1:(n-1),
                   function(i){
                     sum(triangle[!is.na(triangle[, i+1]) & !is.na(triangle[, i]),i+1] * weight[(!is.na(triangle[, i+1]) & !is.na(triangle[, i]))[-n], i]) /
                       sum(triangle[!is.na(triangle[, i+1]) & !is.na(triangle[, i]),i] * weight[(!is.na(triangle[, i+1]) & !is.na(triangle[, i]))[-n], i] )})
  names(lambda) <- colnames(triangle)[2:n]

  # Verify the validity of lambda
  if(!all(!is.na(lambda))){stop("Unable to compute passing coefficients.")}

  # Apply the coefficients to create the developed triangle
  developedTriangle <- triangle
  for(i in 2:n){
    developedTriangle[is.na(developedTriangle[,i]),i] <- developedTriangle[is.na(developedTriangle[,i]),i-1] * lambda[i-1]
  }

  # Calculate IBNR by accident year
  ibnr <- developedTriangle[2:n,n] - diag(developedTriangle[,n:1])[-1]

  # Return the values
  return(list(triangle = triangle,
              developedTriangle = developedTriangle,
              lambdas = lambda,
              ibnrByAccidentYear = ibnr,
              ibnr = sum(ibnr)
  ))
}
