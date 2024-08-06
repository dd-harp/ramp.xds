
#' @title Negative Binomial Force of Infection
#' @description Compute the daily FoI from the daily EIR
#' under a negative binomial model for exposure
#' @details
#' This function computes the local daily FoI, \eqn{h} as a function of
#' the local daily EIR, \eqn{E},
#' under a negative binomial model for the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{h = \phi \log{\frac{1 + b E}\phi}}
#' The expression is derived by solving the expression in [F_ar.nb] for \eqn{E}
#' This negative binomial model uses the **mu** \eqn{= bE} and **size** \eqn{=\phi}
#' parameterization (see [pnbinom]).
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @seealso Related topics: [Exposure.xde] and [F_ar.nb] and [setup_exposure_nb]
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
#' @inheritParams foi2eir
#' @return a [numeric] vector of length `nStrata`
#' @seealso [F_foi.nb]
#' @export
foi2eir.nb <- function(foi, b, pars){
  (exp(foi/pars$ARpar$sz) - 1)*pars$ARpar$sz/b
}

#' @title Negative Binomial Attack Rates
#' @description A negative binomial model for the attack rate as a function of the daily EIR.
#' @details
#' This function computes the daily AR, \eqn{\alpha}, as a function
#' of the daily EIR, \eqn{E} or `eir`,
#' under a negative binomial model for the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{\alpha = 1-(1 + b E/\phi)^{-\phi} }
#' This negative binomial model uses the **mu** \eqn{= bE} and **size** \eqn{=\phi}
#' parameterization (see [pnbinom]).
#' The formula is equivalent to:
#'
#'`pnbinom(0, mu=b*eir, size=phi, lower.tail=FALSE)`
#' @inheritParams F_ar
#' @return a [numeric] vector of length `nStrata`
#' @seealso Related topics: [Exposure.dts], [setup_exposure_nb] & [F_foi.nb]
#' @export
F_ar.nb <- function(eir, b, pars){
  1 - (1+b*eir/pars$ARpar$sz)^(-pars$ARpar$sz)
}

#' @title A negative binomial model for the daily EIR. as a function of the daily attack rate
#' @description Implements [ar2eir] for a negative binomial model
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @seealso [F_ar.nb]
#' @export
ar2eir.nb <- function(ar, b, pars){
 ((1-ar)^(-1/pars$ARpar$sz)-1)*pars$ARpar$sz/b
}

#' @title Set up the negative binomial model of exposure
#' @param pars an `xds` object
#' @param sz the negative binomial size parameter (see [pnbinom])
#' @return an `xds` object
#' @seealso [F_foi.nb] and [F_ar.nb]
#' @export
setup_exposure_nb <- function(pars, sz) {
  UseMethod("setup_exposure_nb", pars$xds)
}

#' @title Set up the negative binomial model of exposure for continuous time models
#' @inheritParams setup_exposure_nb
#' @return an **`xds`** object
#' @seealso Also, see [F_foi.nb]
#' @export
setup_exposure_nb.xde <- function(pars, sz) {
  FoIpar <- list()
  class(FoIpar) <- 'nb'
  FoIpar$sz = sz
  pars$FoIpar <- FoIpar
  pars$FoI    <- list()
  return(pars)
}

#' @title Set up the negative binomial model for exposure for discrete time models
#' @inheritParams setup_exposure_nb
#' @return an **`xds`** object
#' @seealso Also, see [F_ar.nb]
#' @export
setup_exposure_nb.dts <- function(pars, sz) {
  ARpar <- list()
  class(ARpar) <- 'nb'
  ARpar$sz = sz
  pars$ARpar <- ARpar
  pars$AR    <- list()
  return(pars)
}
