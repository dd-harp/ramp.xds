
#' @title The daily FoI as a function of the daily EIR under a negative binomial model of exposure.
#' @description Implements [F_foi] for a negative binomial model
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @export
F_foi.nb <- function(eir, b, pars){
  log(1 + b*eir/pars$FOIpar$sz)*pars$FOIpar$sz
}

#' @title A negative binomial model for the daily FoI as a function of the daily EIR.
#' @description Implements [foi2eir] for a negative binomial model
#' @inheritParams foi2eir
#' @return a [numeric] vector of length `nStrata`
#' @export
foi2eir.nb <- function(foi, b, pars){
  (exp(foi/pars$ARpar$sz) - 1)*pars$ARpar$sz/b
}

#' @title A negative binomial model for the attack rate as a function of the daily EIR.
#' @description Implements [F_ar] for a negative binomial model
#' @inheritParams F_ar
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ar.nb <- function(eir, b, pars){
  1 - (1+b*eir/pars$ARpar$sz)^(-pars$ARpar$sz)
}

#' @title A negative binomial model for the daily EIR. as a function of the daily attack rate
#' @description Implements [ar2eir] for a negative binomial model
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @export
ar2eir.nb <- function(ar, b, pars){
 ((1-ar)^(-1/pars$ARpar$sz)-1)*pars$ARpar$sz/b
}


#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @param sz the size parameter, as in dnbinom(mu=mu, size=size)
#' @return none
#' @export
xde_setup_exposure_nb <- function(pars, sz) {
  FOIpar <- list()
  class(FOIpar) <- 'nb'
  FOIpar$sz = sz
  pars$FOIpar <- FOIpar
  return(pars)
}


#' @title Make parameters for the null model of exposure
#' @param pars a [list]
#' @param sz the size parameter, as in dnbinom(mu=mu, size=size)
#' @return none
#' @export
dts_setup_exposure_nb <- function(pars, sz) {
  ARpar <- list()
  class(ARpar) <- 'nb'
  ARpar$sz = sz
  pars$ARpar <- ARpar
  return(pars)
}
