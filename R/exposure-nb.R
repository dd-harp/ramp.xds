
#' @title A negative binomial model for Exposure
#' @description Compute the daily FoI, \eqn{h},
#' given the daily EIR, \eqn{E},
#' under a negative binomial model for the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{h = \log{1 + b E}/\phi}
#' The model is parameterized using the **mu** \eqn{= E} and **sz** \eqn{= 1/\phi}
#' parameterization.
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @export
F_foi.nb <- function(eir, b, pars){
  log(1 + b*eir/pars$FoIpar$sz)*pars$FoIpar$sz
}

#' @title A negative binomial model for the daily FoI as a function of the daily EIR.
#' @description Implements [foi2eir] for a negative binomial model
#' @details Compute the daily AR, \eqn{h},
#' given the daily EIR, \eqn{E},
#' under a negative binomial model for the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{\alpha = \left(e^{h/\phi} -1\right) \frac \phi b }
#' The model is parameterized using the **mu** \eqn{= E} and **sz** \eqn{= 1/\phi}
#' parameterization. The expression is the complement of the zero term.
#' @inheritParams foi2eir
#' @return a [numeric] vector of length `nStrata`
#' @export
foi2eir.nb <- function(foi, b, pars){
  (exp(foi/pars$ARpar$sz) - 1)*pars$ARpar$sz/b
}

#' @title A negative binomial model for the attack rate as a function of the daily EIR.
#' @description Implements [F_ar] for a negative binomial model
#' @details Compute the daily AR, \eqn{h},
#' given the daily EIR, \eqn{E},
#' under a negative binomial model for the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{\alpha = 1-(1 + b E/\phi)^{-\phi} }
#' The model is parameterized using the **mu** \eqn{= E} and **sz** \eqn{=\phi}
#' parameterization. The expression is the complement of the zero term.
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

#' @title Set up the negative binomial model of exposure
#' @param pars an `xds` object
#' @param sz the size parameter, as in [dnbinom](mu=mu, size=sz)
#' @return the modified `xds` object
#' @export
setup_exposure_nb <- function(pars, sz) {
  UseMethod("setup_exposure_nb", pars$xds)
}

#' @title Set up the negative binomial model of exposure for continuous time models
#' @inheritParams setup_exposure_nb
#' @return the modified **`xds`** object
#' @export
setup_exposure_nb.xde <- function(pars, sz) {
  FoIpar <- list()
  class(FoIpar) <- 'nb'
  FoIpar$sz = sz
  pars$FoIpar <- FoIpar
  pars$FoI    <- list()
  return(pars)
}

#' @title Set up the negative binomial model of exposure for discrete time models
#' @inheritParams setup_exposure_nb
#' @return the modified **`xds`** object
#' @export
setup_exposure_nb.dts <- function(pars, sz) {
  ARpar <- list()
  class(ARpar) <- 'nb'
  ARpar$sz = sz
  pars$ARpar <- ARpar
  pars$AR    <- list()
  return(pars)
}
