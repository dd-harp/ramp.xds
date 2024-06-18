
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


#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @return none
#' @export
setup_exposure_pois <- function(pars) {
  FOIpar <- list()
  class(FOIpar) <- 'pois'
  pars$FOIpar <- FOIpar
  return(pars)
}
