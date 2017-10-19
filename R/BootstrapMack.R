#' Bootstrap Mack
#'
#' \code{BootstrapMack} computes the bootstraped distribution for IBNR with a Mack Model
#'
#' @param triangle Undevelopped triangle as a matrix
#' @param nBoot Number of samples. Default value to 1000
#' @param weight Boolean matrix with 1 row and 1 column less than the triangle to tell if the link ratio is to be considered: 1 for yes, 0 for no
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangleList: the list of bootsraped triangles }
#'   \item{ibnrByAccidentYear: a data frame containing ibnrs by accident year}
#'   \item{ibnr: a vector of the total ibnr values)}
#'   \item{predictionError: estimation of the prediction error on IBNR}}
#'
#' @examples bm <- BootstrapMack(triangleExampleEngland, 1000)
#'
#' @import data.table
#'
#' @export
BootstrapMack <- function(triangle, nBoot = 1000, weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}

  # Scaling
  n0 <- sum(!is.na(triangle))
  p <- 2*n -1

  # Lambda calculation
  outputCL <- ChainLadder(triangle, weight)
  lambda <- outputCL$lambdas

  # Residuals computation
  varMack93 <- Mack93Variance(triangle, weight)
  coeffPassages <- triangle
  coeffPassages[,] <- NA
  coeffPassages[-n, -n] <- triangle[-n,-1]/triangle[-n, -n]
  lambdas <- t(matrix(rep(c(lambda, NA), n), nrow = n))
  #sigmas <- sqrt(t(matrix(rep(1/((n-1):0)*colSums(triangle *(coeffPassages - lambdas)^2, na.rm = TRUE),n), nrow = n)))
  sigmas <- sqrt(t(matrix(rep(c(varMack93$coeffVar, NA), n), nrow = n)))
  sigmas[sigmas == 0] <- 0.001

  res <- sqrt(triangle) * (coeffPassages - lambdas) / sigmas #* sqrt(n0/(n0-p))
  #res[1, n-1] <- 0
  meanRes <- t(matrix(rep(colMeans(res, na.rm = TRUE), n), nrow = n))
  #sigRes <- t(matrix(rep(apply(res, 2, function(x) sd(x, na.rm = TRUE)), n), nrow = n))
  res <- (res - meanRes)

  # Bootstrap function
  functionBoostrap <- function(res){
    resEch <- res
    resEch[!(is.na(res) | res == 0)] <- sample(res[!(is.na(res) | res == 0)], replace = TRUE)
    coeffs <- (resEch) * sigmas / sqrt(triangle) + lambdas
    coeffs[coeffs < 0.1] <- 0.1
    triangleBoot <- triangle
    for(i in (n-1):1){
      triangleBoot[1:(n-i),i] <- triangle[1:(n-i),i+1] / coeffs[1:(n-i),i]
    }
    lambdaBoot <- colSums(triangle * coeffs, na.rm = TRUE) / colSums(triangle * !is.na(coeffs), na.rm = TRUE)
    # lambdaBoot <- ChainLadder(triangleBoot)$lambdas
    # sigmaBoot <- sqrt(Mack93Variance(triangleBoot)$coeffVar)
    sigmaBoot <- sigmas[1, -n]
    for(i in 1:(n-2)){
      meanNorm <- diag(triangleBoot[n:i,i:n])[1:(n-i)] * lambdaBoot[i:(n-1)]
      sigmaNorm <- sigmaBoot[i:(n-1)] * sqrt(diag(triangleBoot[n:i,i:n])[1:(n-i)])
      sigmaNorm[sigmaNorm < 0] <- 0
      diag(triangleBoot[n:(i+1),(i+1):n]) <- sapply(1:(n-i), function(j) max(0.1, rnorm(1, meanNorm[j], sigmaNorm[j])))
    }
    triangleBoot[n,n] <-  rnorm(1, triangleBoot[n,n-1] * lambdaBoot[n-1], sigmaBoot[n-1] * sqrt(triangleBoot[n,n-1]))
    return(triangleBoot)
  }

  triangleList <- lapply(1:nBoot, function(x) {functionBoostrap(res)})
  psapByAccidentYear <- lapply(triangleList, function(x) data.frame(matrix(x[, n] - rev(diag(x[n:1,])), nrow = 1)))
  psapByAccidentYear <- rbindlist(psapByAccidentYear)
  colnames(psapByAccidentYear) <- rownames(triangle)[-1]
  psap <- rowSums(psapByAccidentYear)
  predictionError <- sd(psap)
  return(list(triangleList = triangleList,
              ibnrByAccidentYear = psapByAccidentYear,
              ibnr = psap,
              predictionError = predictionError))
}
