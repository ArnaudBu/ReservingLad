#' Chain Ladder
#'
#' \code{ChainLadder} applies the Chain Ladder method to a cumulated claim triangle.
#'
#' @param triangle Undevelopped triangle as a matrix
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
ChainLadder <- function(triangle){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}

  # Compute the passing coefficients
  lambda <- sapply(1:(n-1),
                   function(i){
                     sum(triangle[!is.na(triangle[, i+1]) & !is.na(triangle[, i]),i+1]) /
                       sum(triangle[!is.na(triangle[, i+1]) & !is.na(triangle[, i]),i])})
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
