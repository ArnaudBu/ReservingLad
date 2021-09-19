#' Age to Age factors impacts
#'
#' \code{AtaImpact} computes the impact on the total reserve from removing each age to age factor in the Chain Ladder estimation for coefficents.
#'
#' @param triangle Cumulated triangle as a matrix
#' @param mode Character. The way to display the impact: as a value ("value") or as a percent of the total reserve ("percent").
#' @return A triangle equivalent to the age-to-age factors triangle, but with the impact on the reserves instead.
#'
#' @examples  impact <- AtaImpact(triangleExampleEngland)
#'
#' @export
AtaImpact <- function(triangle, mode = "value"){

  # Validity of triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("triangle is not a numeric matrix")}
  # Validity of the mode
  if(!(mode %in% c("value", "percent"))) stop("Invalid mode")

  # Compute weight matrix
  weight <- !is.na(triangle)

  # Computation of each impact
  outputCLref <- ChainLadder(triangle)
  impactuni <- function(x){
    weightuni <- weight
    weightuni[x] <- 0
    outputCL <- ChainLadder(triangle, weightuni)
    if(mode == "value"){
    return(round(outputCL$ibnr - outputCLref$ibnr))
    } else {
      return(round((outputCL$ibnr - outputCLref$ibnr)/abs(outputCLref$ibnr)*100, 2))
    }
  }
  impacts <- sapply(which(!is.na(weight)), impactuni)

  # Output
  output <- weight
  output[!is.na(output)] <- impacts
  return(output)
}
