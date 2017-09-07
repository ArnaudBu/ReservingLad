#' Decumulates a triangle
#'
#' \code{Decumulate} decumulates a triangle
#'
#' @param triangle Undevelopped triangle as a matrix
#' @return The decumulated triangle as a matrix
#'
#' @examples  decTriangle <- Decumulate(triangleExampleEngland)
#'
#' @export
Decumulate <- function(triangle){

  # Validity of triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("triangle is not a numeric matrix")}

  # Decumulated triangle
  decTriangle <- triangle
  decTriangle[,-1] <- t(apply(triangle, 1, diff))

  # Return triangle
  return(decTriangle)
}
