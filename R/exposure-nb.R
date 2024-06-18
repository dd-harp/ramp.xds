
#' @title The daily FoI as a function of the daily EIR under a negative binomial model of exposure.
#' @description Implements [F_foi] for a negative binomial model
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @export
F_foi.nb <- function(eir, b, pars){
  log(1 + b*eir/pars$FOIpar$sz)*pars$FOIpar$sz
}

#' @title The daily FoI as a function of the daily EIR under a negative binomial model of exposure.
#' @description Implements [F_ar] for a negative binomial model
#' @inheritParams F_ar
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ar.nb <- function(eir, b, pars){
  1 - (1+b*eir/pars$FOIpar$sz)^(-pars$FOIpar$sz)
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
