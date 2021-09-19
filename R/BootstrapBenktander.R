#' Bootstrap
#'
#' \code{BootstrapBenktander} computes the bootstraped distribution for IBNR on a Benktander method.
#'
#' @param triangle CUmulated triangle as a matrix. The matrix should be square
#' @param ultimateClaims Ultimate claims by accident year.
#' @param weight Boolean matrix the same size of the triangle to tell if the value is to be considered: 1 for yes, 0 for no. First column is not considered
#' @param nBoot Number of samples. Default value equals to 1000.
#' @return A list containing the following objects:
#' \itemize{
#'   \item{triangleList: the list of bootsraped triangles }
#'   \item{ibnrByAccidentYear: a data frame containing ibnrs by accident year}
#'   \item{ibnr: a vector of the total ibnr values)}
#'   \item{predictionError: estimation of the prediction error on IBNR}}
#'
#' @examples ultimateClaims <- c(3901463,5433719,5378826,5297906,4858200,5111171,5660771,6784799,5642266,4969825)
#' @examples bbt <- BootstrapBenktander(triangleExampleEngland, ultimateClaims, 1000)
#'
#' @export
BootstrapBenktander <- function(triangle,  ultimateClaims, nBoot = 1000, weight = NA){

  # Validity checks for the triangle
  if(!(is.matrix(triangle) & is.numeric(triangle))){stop("The triangle is not a numeric matrix.")}
  if(nrow(triangle) != ncol(triangle)){stop("Number of rows different of number of columns in the triangle.")}
  n <- nrow(triangle)
  if(!all(!is.na(diag(triangle[n:1,])))){stop("Diagonal contains NA values.")}

  # Scaling
  n0 <- sum(!is.na(triangle))
  p <- 2*n -1

  # triangle calculation
  outputBK <- Benktander(triangle, ultimateClaims, weight = weight)
  outputBF <- BornFerg(triangle, ultimateClaims, weight = weight)
  gamma <- outputBF$gammas
  ulti <- outputBF$developedTriangle[,ncol(triangle)]
  ultiBK <- outputBK$developedTriangle[,ncol(triangle)]

  # Recalculated triangle
  triangleRecalc <- triangle
  n <- ncol(triangle)
  for(i in (n-1):1){
    triangleRecalc[1:(n-i+1),i] <- ultiBK[1:(n-i+1)] - (1-gamma[i]) * ulti[1:(n-i+1)]
  }

  if(!all(triangleRecalc > 0, na.rm = TRUE)) warning("Negative values in the recalculated triangle may lead to wrong results.")


  # Decumulated triangles
  decTriangle <- Decumulate(triangle)
  decTriangleRecalc <- Decumulate(triangleRecalc)

  # Calculation of pearson residuals
  res <- (decTriangle - decTriangleRecalc) / ifelse(decTriangleRecalc == 0, 1,sqrt(abs(decTriangleRecalc)))
  phi <- colMeans(res^2, na.rm = TRUE) *n0/ (n0-p)
  phiprime <- sum(res^2, na.rm = TRUE) / (n0-p)
  phi[phi==0] <- 0.000001
  phi <- t(matrix(rep(phi, n), nrow = n))
  res <- res / sqrt(phi)  #*sqrt(n0/(n0-p))

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
  psapByAccidentYear <- rbindlist(psapByAccidentYear)
  colnames(psapByAccidentYear) <- rownames(triangle)
  psap <- rowSums(psapByAccidentYear)
  predictionError <- sqrt(phiprime*mean(psap) + n0/(n0-p) * sd(psap)^2)
  return(list(triangleList = triangleList,
              ibnrByAccidentYear = psapByAccidentYear,
              ibnr = psap,
              predictionError = predictionError))
}
