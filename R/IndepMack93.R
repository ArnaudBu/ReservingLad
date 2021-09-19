#' Mack93 independence
#'
#' \code{IndepMack93} test the hypothesis of the independence of the development factors across
#' calendar years.
#'
#' @param triangle Cumulated triangle as a matrix
#' @param range Range used for hypothesis testing.
#' @return  A list containing the following objects:
#' \itemize{
#'   \item{hypTab: the table for hypothesis testing }
#'   \item{valueTest: the values for hypothesis testing as a data.frame}
#'   \item{validated: boolean. If the test is validated with the selected range}}
#'
#' @details Implementation of the technique for error estimation developed by Mack in its 1993 article "MEASURING THE VARIABILITY
#' OF CHAIN LADDER RESERVE ESTIMATES"
#'
#' @examples error <- IndepMack93(triangleExampleEngland)
#'
#' @export
IndepMack93 <- function(triangle, range = 0.95){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(!(range <1 & range > 0)){stop("Invalid range.")}

  # Construction of the age to age coefficients
  n <- ncol(triangle)
  coeffsAta <- triangle[,-1] / triangle[, -n]
  
  # Superior and Lower matrices
  S <- apply(coeffsAta, 2, function(x) x > median(x, na.rm = TRUE))
  L <- apply(coeffsAta, 2, function(x) x < median(x, na.rm = TRUE))
  
  # Diagonals matrices
  J <- col(coeffsAta) + row(coeffsAta)
  J[is.na(coeffsAta)] <- NA
  
  # Summing by diagonal
  hypTab <- data.frame()
  for(j in min(J, na.rm = TRUE):max(J, na.rm = TRUE)){
    temp = data.frame(j = j, S = sum(S[J==j], na.rm = TRUE), L = sum(L[J==j], na.rm = TRUE))
    hypTab <- rbind(hypTab, temp)
  }
  
  # Construction of the table
  hypTab$n <- hypTab$S + hypTab$L
  hypTab$m <- floor((hypTab$n -1)/2)
  hypTab$Z <- pmin(hypTab$S, hypTab$L)
  hypTab$EZ <- hypTab$n / 2 - choose(hypTab$n -1, hypTab$m) * hypTab$n / 2^hypTab$n
  hypTab$VarZ <- hypTab$n * (hypTab$n - 1) / 4 - choose(hypTab$n -1, hypTab$m) * hypTab$n * (hypTab$n - 1) / 2^hypTab$n + hypTab$EZ - hypTab$EZ^2
  
  
  # Output
  valueTest <- data.frame(Z = sum(hypTab$Z), EZ = sum(hypTab$EZ), SdZ = sqrt(sum(hypTab$VarZ)))
  return(list(hypTab = hypTab,
              valueTest = valueTest,
              validated = abs(valueTest$Z - valueTest$EZ) < qnorm(1 / 2 +  range / 2) * valueTest$SdZ))
}
