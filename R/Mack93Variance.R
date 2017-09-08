#' Mack93 variances
#'
#' \code{Mack93Variance} computes the variances coefficients for each devellopment year
#'
#' @param triangle Undevelopped triangle as a matrix
#' @return A list containing the following objects:
#' \itemize{
#'   \item{coeffVar: variances on coefficients }
#'   \item{ibnrVar: variances on ibnr by accident year}
#'   \item{ibnrVarTot: total variance on ibnr}}
#'
#' @details Implementation of the technique for error estimation developed by Mack in its 1993 article.
#'
#' @examples error <- Mack93Variance(triangleExampleEngland)
#'
#' @export
Mack93Variance <- function(triangle){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}


  # Application of ChainLadder
  outputCL <- ChainLadder(triangle)

  # Construction of lambda estimators variance
  n <- ncol(triangle)
  coeffVar <- outputCL$lambdas
  for(i in 1:(n-2)){
    coeffVar[i] <- 1 / (n - i - 1) * sum( triangle[, i] * (triangle[,i+1]/triangle[,i] - outputCL$lambdas[i])^2, na.rm = TRUE)
  }
  if(coeffVar[n-3] != 0){
    coeffVar[n-1] <- min(coeffVar[n-2]^2/coeffVar[n-3], min(coeffVar[n-2], coeffVar[n-3]))
  } else{
    coeffVar[n-1] <- min(coeffVar[n-2], coeffVar[n-3])
  }

  # Quadratic error on IBNR
  ibnrVar <- coeffVar
  vecVar <- coeffVar
  devTriangle <- outputCL$developedTriangle
  triangleMirror <- triangle
  diag(triangleMirror[nrow(triangleMirror):1,]) <- NA
  for(i in 1:(n-1)){
    ibnrVar[i] <- devTriangle[i+1,n]^2 * sum(coeffVar[(n-i):(n-1)] / outputCL$lambdas[(n-i):(n-1)]^2 * ( 1 / devTriangle[i+1,(n-i):(n-1)] + 1 / colSums(triangleMirror, na.rm = TRUE)[(n-i):(n-1)]))
    if(i+1 < n){
      vecVar[i] <- devTriangle[i+1,n] * sum(devTriangle[(i+2):n, n]) * sum(2* coeffVar[(n-i):(n-1)] / outputCL$lambdas[(n-i):(n-1)]^2 /colSums(triangleMirror, na.rm = TRUE)[(n-i):(n-1)])
    }
  }
  vecVar[n-1] <- 0

  ibnrVarTot <- sum(ibnrVar + vecVar)

  # Return the output
  names(ibnrVar) <- rownames(triangle[-1,])
  return(list(coeffVar = coeffVar,
              ibnrVar = ibnrVar,
              ibnrVarTot = ibnrVarTot
  ))
}
