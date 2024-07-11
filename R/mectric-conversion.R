
#' @title Convert the EIR into a vector describing infective biting density
#' @description Computes a vector of length `nStrata` describing the daily eir, per patch from
#' a vector of length `nPatches` describing the daily infective biting density.
#' \deqn{fqZ = \left(\beta^T \cdot \beta \right)^{-1} \cdot \beta^T \cdot E}
#' @param eir a vector describing the EIR in several strata
#' @param beta the mixing matrix
#' @importFrom MASS ginv
#' @return a numeric [vector]
#' @export
eir2fqZ <- function(eir, beta){
  stopifnot(inherits(beta, 'matrix'))
  stopifnot(is.double(eir))

  BBinv <- ginv(t(beta) %*% beta)
  fqZ <- BBinv %*% t(beta) %*% eir
  return(fqZ)
}

#' @title  Convert a vector describing infective biting density into the EIR, \eqn{E}
#' @description Computes a vector of length `nPatches` describing infective biting density, per patch, from
#' a vector of length `n` describing the daily EIR per stratum:
#' \deqn{E = \beta \cdot fqZ}
#' @param fqZ a vector describing infective biting density
#' @param beta the mixing matrix
#' @importFrom MASS ginv
#' @return a numeric [vector]
#' @export
fqZ2eir <- function(fqZ, beta){
  stopifnot(inherits(beta, 'matrix'))
  stopifnot(is.double(eir))

  eir <- beta %*% fqZ
  return(eir)
}
