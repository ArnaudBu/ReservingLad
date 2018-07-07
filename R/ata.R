#' Age to Age factors
#'
#' \code{ata} computes the age to age coefficients for a triangle
#'
#' @param triangle Cumulated triangle as a matrix
#' @return The age to age factors
#'
#' @examples  links <- ata(triangleExampleEngland)
#'
#' @export
ata <- function(triangle){

  # Validity of triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("triangle is not a numeric matrix")}

  # Compute the factors
  a <- triangle[, -1] / triangle[, -ncol(triangle)]
  colnames(a) <- paste(1:(ncol(triangle)-1), 2:ncol(triangle), sep = "->")

  # Return the factors
  return(a)
}
