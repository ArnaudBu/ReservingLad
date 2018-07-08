#' Benktander Reserving method
#'
#' \code{Benktander} applies the Benktander method to a cumulated claim triangle, as described in this paper: https://www.casact.org/library/astin/vol30no2/333.pdf.
#'
#' @param triangle Undevelopped triangle as a matrix.
#' @param ultimateClaims Ultimate claims by accident year.
#' @param weight  Boolean matrix with 1 row and 1 column less than the triangle to tell if the link ratio is to be considered: 1 for yes, 0 for no.
#'
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangle: the input triangle }
#'   \item{developedTriangle: the developed triangle}
#'   \item{gammas: the Benktander coefficients}
#'   \item{ibnrByAccidentYear: ibnr reserve by accident year}
#'   \item{ibnr: total ibnr reserve}}
#' @details Missing values are handled. There just need to be replaced by a NA.
#' @details It is possible to have more than one complete line.
#'
#' @examples ultimateClaims <- c(3901463,5433719,5378826,5297906,4858200,5111171,5660771,6784799,5642266,4969825)
#' @examples outputBT <- Benktander(triangleExampleEngland, ultimateClaims)
#'
#' @export
Benktander <- function(triangle, ultimateClaims, weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  n <- nrow(triangle)
  p <- ncol(triangle)

  # Validity checks for the ultimate claims
  if(!(is.numeric(ultimateClaims) & length(ultimateClaims) == n)){stop("Unvalid ultimate. Unable to apply coefficients")}

  # Apply Bornhuetter Fergusson model
  bf <- BornFerg(triangle, ultimateClaims, coeffs = "CL", weight = weight)

  # Computations
  reserve <- bf$developedTriangle[,ncol(bf$developedTriangle)] * rev(1- bf$gammas)
  ultimate <- latest(triangle) + reserve
  developedTriangle <- triangle
  developedTriangle[, ncol(developedTriangle)] <- ultimate
  for(i in 2:(ncol(triangle)-1)){
    developedTriangle[is.na(developedTriangle[,i]),i] <- ultimate[is.na(developedTriangle[,i])] - (1-bf$gammas[i]) *
      bf$developedTriangle[is.na(developedTriangle[,i]),ncol(developedTriangle)]
  }

  # Output
  return(list(triangle = triangle,
              developedTriangle = developedTriangle,
              gammas = bf$gammas,
              ibnrByAccidentYear = reserve[-1],
              ibnr = sum(reserve),
              input_ultimate = ultimateClaims
  ))
}
