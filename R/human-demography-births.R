
#' @title Derivatives of demographic changes in human populations
#' @description Implements [Births] for static population models
#' @inheritParams Births
#' @return a [numeric] vector of zeros
#' @export
Births.zero <- function(t, y, Hpar){
  0*y
}

#' @title Derivatives of demographic changes in human populations
#' @description Implements [Births] for static population models
#' @inheritParams Births
#' @return a [numeric] vector of zeros
#' @export
Births.static <- function(t, y, Hpar){
  0*y + Hpar$birth_rate
}

#' @title Setup a static birth_rate
#' @description Each model determines the compartment for
#' births. The birth rate should be a vector of length `nStrata`
#' where the entries are zero for all but the stratum that
#' gets newborns.
#' @param pars a [list]
#' @param i the host species index
#' @param birth_rate a birth rate vector
#' @return an **`xds`** object
#' @export
setup_births_static <- function(pars, i, birth_rate) {
  stopifnot(length(birth_rate) == pars$nStrata[i])
  Bf <- list()
  class(Bf) <- "static"
  Bf$birth_rate <- birth_rate
  pars$Hpar[[1]]$Bf <- Bf

  return(pars)
}
