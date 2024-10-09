
#' @title Compute Demographic Changes
#' @description Return a vector of zeros
#' @inheritParams dHdt
#' @return a [numeric] vector of 0s
#' @export
dHdt.zero <- function(t, y, Hpar){
  0*y
}

#' @title Derivatives of demographic changes in human populations
#' @description Implements [dHdt] when `y` is static
#' @inheritParams dHdt
#' @return a [numeric] vector of 0s
#' @export
dHdt.matrix <- function(t, y, Hpar){
  Hpar$Hmatrix %*% y
}

#' @title Setup a matrix for `dHdt`
#' @description A demographic matrix should be a
#' square with dimensions \eqn{n_h \times n_h},
#' where \eqn{n_n=} `nStrata.` The column sums give
#' the background death rate for individuals
#' in the stratum.
#' @param pars a [list]
#' @param i the host species index
#' @param Hmatrix a demographic matrix
#' @return an **`xds`** object
#' @export
setup_Hmatrix <- function(pars, i, Hmatrix) {
  stopifnot(dim(Hmatrix) == c(pars$nStrata[i], pars$nStrata[i]))
  dH <- list()
  class(dH) <- "matrix"
  dH$Hmatrix <- Hmatrix
  pars$Hpar[[1]]$dH <- dH

  return(pars)
}
