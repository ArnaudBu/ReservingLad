#' Latest Observations
#'
#' \code{latest} extracts the latests observations (payments) from a triangle.
#'
#' @param triangle Cumulated triangle as a matrix
#' @return The latest observation
#'
#' @examples  l <- latest(triangleExampleEngland)
#'
#' @export
latest <- function(triangle){

  # Validity of triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("triangle is not a numeric matrix")}

  # Extract the latest payments
  l <- apply(triangle, 1, function(x) tail(x[!is.na(x)], 1))
  names(l) <- rownames(triangle)

  # Return the payments
  return(l)
}
