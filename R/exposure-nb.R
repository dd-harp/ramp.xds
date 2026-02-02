
#' @title Set up a nbson model for Exposure and Infection
#' 
#' @inheritParams setup_exposure
#' 
#' @return an  **`xds`** object
#' @export
#' @seealso Also, see [F_foi.nb] and [F_ar.nb]
#' @keywords internal
#'
setup_exposure.nb <- function(EHname, xds_obj, i=1, options=list()) {
  xds_obj$XY_interface$env_het_obj[[i]] = make_exposure_nb(xds_obj$nStrata, options)
  return(xds_obj)
}

#' @title Make a nbson Exposure Model Object 
#' 
#' @description Set up the nbson model for
#' exposure for continuous time models
#'
#' @param nStrata the number of population strata
#' @param options options to configure negative binomial exposure
#' @param sz the `size` parameter for a negative binomial distribution
#' 
#' @return a local exposure model object
#' 
#' @seealso Also, see [F_foi.nb]
#' @export
make_exposure_nb <- function(nStrata, options, sz=1) {
  local_exposure_object <- list()
  class(local_exposure_object) <- 'nb'
  local_exposure_object$sz = with(options, sz) 
  return(local_exposure_object)
}

#' @title Negative Binomial Exposure 
#' 
#' @description Compute the daily FoI from the daily EIR
#' under a negative binomial model for exposure
#' 
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
#' @seealso Related topics: [Exposure.xde] and [F_ar.nb] and [make_exposure_nb]
#' @export
#' @keywords internal
F_foi.nb <- function(eir, b, env_het_obj){with(env_het_obj,{
  log(1 + b*eir/sz)*sz
})}

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
#' @keywords internal
foi2eir.nb <- function(foi, b, env_het_obj){with(env_het_obj,{
  (exp(foi/sz) - 1)*sz/b
})}

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
#' @seealso Related topics: [Exposure.dts], [make_exposure_nb] & [F_foi.nb]
#' @export
#' @keywords internal
F_ar.nb <- function(eir, b, env_het_obj){with(env_het_obj,{
  1 - (1+b*eir/sz)^(-sz)
})}

#' @title A negative binomial model for the daily EIR. as a function of the daily attack rate
#' @description Implements [ar2eir] for a negative binomial model
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @seealso [F_ar.nb]
#' @export
#' @keywords internal
ar2eir.nb <- function(ar, b, env_het_obj){with(env_het_obj,{
 ((1-ar)^(-1/sz)-1)*sz/b
})}
