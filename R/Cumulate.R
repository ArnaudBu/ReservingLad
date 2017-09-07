#' Cumulates a triangle
#'
#' \code{Cumulate} Cumulates a triangle
#'
#' @param triangle Undevelopped triangle as a matrix
#' @return The cumulated triangle as a matrix
#'
#' @examples  decTriangle <- Decumulate(triangleExampleEngland)
#' @examples  cumTriangle <- Cumulate(triangleExampleEngland)
#'
#' @export
Cumulate <- function(triangle){

  # Validity of triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("triangle is not a numeric matrix")}

  # Cumulated triangle
  cumTriangle <- triangle
  cumTriangle <- t(apply(cumTriangle, 1, cumsum))

  # Return triangle
  return(cumTriangle)
}
