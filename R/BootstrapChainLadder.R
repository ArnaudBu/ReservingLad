#' Bootstrap
#'
#' \code{BootstrapChainLadder} computes the bootstraped distribution for IBNR with an ODP mehtod.
#'
#' @param triangle Cumulated triangle as a matrix. The matrix should be square
#' @param weight Boolean matrix the same size of the triangle to tell if the value is to be considered: 1 for yes, 0 for no. First column is not considered
#' @param nBoot Number of samples. Default value equals to 1000.
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangleList: the list of bootsraped triangles }
#'   \item{ibnrByAccidentYear: a data frame containing ibnrs by accident year}
#'   \item{ibnr: a vector of the total ibnr values)}
#'   \item{predictionError: estimation of the prediction error on IBNR}}
#'
#' @examples bcl <- BootstrapChainLadder(triangleExampleEngland, 1000)
#'
#' @export
BootstrapChainLadder <- function(triangle, nBoot = 1000, weight = NA){

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

  # Recalculated triangle
  triangleRecalc <- triangle
  n <- ncol(triangle)
  for(i in (n-1):1){
    triangleRecalc[1:(n-i),i] <- triangleRecalc[1:(n-i),i+1] / lambda[i]
  }

  # Decumulated triangles
  decTriangle <- Decumulate(triangle)
  decTriangleRecalc <- Decumulate(triangleRecalc)

  # Calculation of pearson residuals
  res <- (decTriangle - decTriangleRecalc) / ifelse(decTriangleRecalc == 0, 1,sqrt(abs(decTriangleRecalc)))
  phi <- colMeans(res^2, na.rm = TRUE) *n0/ (n0-p)
  phiprime <- sum(res^2, na.rm = TRUE) / (n0-p)
  phi[phi==0] <- 0.000001
  phi <- t(matrix(rep(phi, n), nrow = n))
  res <- res / sqrt(phi)

  # Bootstrap function
  functionBoostrap <- function(res, decTriangleRecalc){
    resEch <- res
    resEch[!(is.na(res))] <- sample(res[!(is.na(res))], replace = TRUE)
    resEch[n:1,][lower.tri(resEch[n:1,], diag = TRUE)][is.na(resEch[n:1,][lower.tri(resEch[n:1,], diag = TRUE)])] <- 0
    triangleBoot <- Cumulate(resEch * sqrt(phi) * sqrt(abs(decTriangleRecalc)) + decTriangleRecalc)
    diag(triangleBoot[n:1,])[is.na(diag(triangleBoot[n:1,]))] <- diag(triangle[n:1,])[is.na(diag(triangleBoot[n:1,]))]
    return(triangleBoot)
  }
  functionSimulate <- function(devDecTri){
    subs <- is.na(triangle)
    devDecTri[subs] <- sapply(1:sum(subs), function(i) ifelse(devDecTri[subs][i]>0,rgamma(n = 1, shape = devDecTri[subs][i] / phi[subs][i], scale = phi[subs][i]), devDecTri[subs][i]))
    return(devDecTri)
  }

  triangleList <- lapply(1:nBoot, function(x) {functionBoostrap(res, decTriangleRecalc)})
  triangleDecList <- lapply(triangleList, function(x) functionSimulate(Decumulate(ChainLadder(x)$developedTriangle)))
  psapByAccidentYear <- lapply(triangleDecList, function(x) data.frame(matrix(rowSums(is.na(triangle) * x), nrow = 1)))
  psapByAccidentYear <- data.table::rbindlist(psapByAccidentYear)
  colnames(psapByAccidentYear) <- rownames(triangle)
  psap <- rowSums(psapByAccidentYear)
  predictionError <- sqrt(phiprime*mean(psap) + n0/(n0-p) * sd(psap)^2)
  return(list(triangleList = triangleList,
              ibnrByAccidentYear = psapByAccidentYear,
              ibnr = psap,
              predictionError = predictionError))
}
