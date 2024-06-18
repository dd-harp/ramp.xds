
#' @title A model for daily FoI as a function of the daily EIR.
#' @description This function compute the daily local FoI as a function
#' of the daily EIR and effects of partial immunity.
#' It is computed based on a probabilistic model of exposure and
#' possibly including environmental heterogeneity. If a model of human / host
#' infection has terms that describe partial immunity, *e.g.* affecting
#' pre-erythrocytic immunity to malaria called by [F_b], those effects are implemented here.
#' The method dispatches on the type of `pars$FOIpar`.
#' @param eir the daily eir for each stratum
#' @param b the probability of infection, per bite
#' @param pars the model object as a [list]
#' @return the daily, local force of infection as a [numeric] vector
#' @export
F_ar <- function(eir, b, pars){
  UseMethod("F_ar", pars$FOIpar)
}

#' @title A Poisson model for the daily local FoI as a function of the daily EIR.
#' @description Implements [F_ar] for a Poisson model
#' @inheritParams F_ar
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ar.pois <- function(eir, b, pars){
  b*eir
}

#' @title The daily FoI as a function of the daily EIR under a negative binomial model of exposure.
#' @description Implements [F_ar] for a negative binomial model
#' @inheritParams F_ar
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ar.nb <- function(eir, b, pars){
  log(1 + b*eir/pars$FOIpar$sz)*pars$FOIpar$sz
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

#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @param sz the size parameter, as in dnbinom(mu=mu, size=size)
#' @return none
#' @export
setup_exposure_nb <- function(pars, sz) {
  FOIpar <- list()
  class(FOIpar) <- 'nb'
  FOIpar$sz = sz
  pars$FOIpar <- FOIpar
  return(pars)
}
