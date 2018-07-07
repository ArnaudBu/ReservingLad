#' Bornhuetter Fergusson
#'
#' \code{BornFerg} applies the Bornhuetter Fergusson method to a cumulated claim triangle.
#'
#' @param triangle Undevelopped triangle as a matrix.
#' @param ultimateClaims Ultimate claims by accident year.
#' @param coeffs Character. Method for computing the coefficients:
#' \itemize{
#'   \item{CL: Same coefficient as Chain Ladder}
#'   \item{full: Take the coefficient computed from full lines}}
#' @param weight Used only if coeff = "CL". Boolean matrix with 1 row and 1 column less than the triangle to tell if the link ratio is to be considered: 1 for yes, 0 for no.
#'
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangle: the input triangle }
#'   \item{developedTriangle: the developed triangle}
#'   \item{gammas: the Bornhuetter Fergusson coefficients}
#'   \item{ibnrByAccidentYear: ibnr reserve by accident year}
#'   \item{ibnr: total ibnr reserve}}
#' @details Missing values are handled. There just need to be replaced by a NA.
#' @details It is possible to have more than one complete line.
#'
#' @examples ultimateClaims <- c(3901463,5433719,5378826,5297906,4858200,5111171,5660771,6784799,5642266,4969825)
#' @examples outputBF <- BornFerg(triangleExampleEngland, ultimateClaims)
#'
#' @export
BornFerg <- function(triangle, ultimateClaims, coeffs = "CL", weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  n <- nrow(triangle)
  p <- ncol(triangle)

  # Validity checks for the ultimate claims
  if(!(is.numeric(ultimateClaims) & length(ultimateClaims) == n)){stop("Unvalid ultimate. Unable to apply coefficients")}

  # Compute the  coefficients
  rows <- which(!is.na(rowSums(triangle)))
  if(coeffs == "CL"){
    outputCL <- ChainLadder(triangle, weight)
    gamma <- c(1, cumprod(outputCL$lambdas))/prod(outputCL$lambdas)
  } else{
    if(length(rows) > 1){
      gamma <- colSums(triangle[rows,]) / sum(triangle[rows, p])
    } else{
      gamma <- triangle[rows,] / triangle[rows, ncol(triangle)]
    }
  }
  names(gamma) <- colnames(triangle)

  # Verify the validity of gamma
  if(!all(!is.na(gamma))){stop("Unable to compute passing coefficients.")}

  # Apply the coefficients to create the developed triangle
  developedTriangle <- triangle
  for(i in (max(rows)+1):n){
    developedTriangle[i, is.na(developedTriangle[i,])] <- developedTriangle[i, min(which(is.na(developedTriangle[i,]))) -1] + (gamma[is.na(developedTriangle[i,])] - gamma[min(which(is.na(developedTriangle[i,]))) -1]) * ultimateClaims[i]
  }

  # Calculate IBNR by accident year
  ibnr <- developedTriangle[(max(rows)+1):n,p] - diag(developedTriangle[(max(rows)):n,p:1])[-1]

  # Return the values
  return(list(triangle = triangle,
              developedTriangle = developedTriangle,
              gammas = gamma,
              ibnrByAccidentYear = ibnr,
              ibnr = sum(ibnr),
              input_ultimate = ultimateClaims
  ))
}
