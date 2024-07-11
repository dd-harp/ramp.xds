
#' @title A Poisson model for the daily local FoI as a function of the daily EIR.
#' @description Implements [F_foi] for a Poisson model
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @export
F_foi.pois <- function(eir, b, pars){
  b*eir
}

#' @title A Poisson model for the daily attack rate as a function of the daily EIR.
#' @description Implements [F_ar] for a Poisson model
#' @inheritParams F_ar
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ar.pois <- function(eir, b, pars){
  1 - exp(-b*eir)
}

#' @title A Poisson model for the daily attack rate as a function of the daily EIR.
#' @description Implements [ar2eir] for a Poisson model
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @export
ar2eir.pois <- function(ar, b, pars){
  -log(1-ar)/b
}

#' @title A Poisson model for the daily attack rate as a function of the daily EIR.
#' @description Implements [foi2eir] for a Poisson model
#' @inheritParams foi2eir
#' @return a [numeric] vector of length `nStrata`
#' @export
foi2eir.pois <- function(foi, b, pars){
   foi/b
}

#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @return none
#' @export
xde_setup_exposure_pois <- function(pars) {
  FOIpar <- list()
  class(FOIpar) <- 'pois'
  pars$FOIpar <- FOIpar
  return(pars)
}

#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @return none
#' @export
dts_setup_exposure_pois <- function(pars) {
  ARpar <- list()
  class(ARpar) <- 'pois'
  pars$ARpar <- ARpar
  return(pars)
}
