
#' @title Poisson Force of Infection
#' @description A Poisson model for the force of infection as a function of the daily EIR.
#' @details This function computes the daily FoI (\eqn{h})
#' as function of the local daily EIR (\eqn{E})
#' under a Poisson model for the distribution
#' of bites per person, and the probability of
#' infection per infective bite (\eqn{b}):
#' \deqn{h = b E}
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @seealso Related: [F_foi] & [F_ar.pois] & [foi2eir.pois]. Called from [Exposure.xde]
#' @export
F_foi.pois <- function(eir, b, pars){
  b*eir
}

#' @title Poisson Attack Rates
#' @description A Poisson model for attack rates as a function of the daily EIR.
#' @details This function computes the daily attack rates (\eqn{\alpha}), the fraction
#' of a cohort that gets infected in a day,
#' as a function of the daily EIR (\eqn{E})
#' under a Poisson model for the distribution
#' of bites per person, and the probability of
#' infection per infective bite (\eqn{b}):
#' \deqn{\alpha = 1 - e^{- b E}}
#' The expression is equivalent to
#'
#' `ppois(0, b*eir, lower.tail=FALSE)`
#' @inheritParams F_ar
#' @return a [numeric] vector: attack rates for the population strata
#' @seealso Related: [F_foi] & [F_foi.pois] & [ar2eir.pois]. Called from [Exposure.dts]
#' @export
F_ar.pois <- function(eir, b, pars){
  1 - exp(-b*eir)
}

#' @title Convert AR to EIR under a Poisson model for Exposure
#' @description The inverse of [F_ar.pois]
#' is \deqn{E = -\frac{\log(1-ar)}{b}}
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @seealso Also, see [F_ar.pois]
#' @export
ar2eir.pois <- function(ar, b, pars){
  -log(1-ar)/b
}

#' @title Convert FoI to EIR under a Poisson model for Exposure
#' @description The inverse of [F_foi.pois] is \eqn{E=h/b}
#' @inheritParams foi2eir
#' @return dEIR as a [numeric] vector of length \eqn{n_h=}`nStrata`
#' @seealso Also, see [F_foi.pois]
#' @export
foi2eir.pois <- function(foi, b, pars){
   foi/b
}

#' @title Set up a Poisson model for Exposure and Infection
#' @param pars an **`xds`** object
#' @return an  **`xds`** object
#' @export
#' @seealso Also, see [F_foi.pois] and [F_ar.pois]
#'
setup_exposure_pois <- function(pars) {
  UseMethod("setup_exposure_pois", pars$xds)
}

#' @title Set up Poisson Exposure and Infection for `xde`
#' @description Set up the Poisson model for
#' exposure for continuous time models
#' @param pars an **`xds`** object
#' @return an  **`xds`** object
#' @seealso Also, see [F_foi.pois]
#' @export
setup_exposure_pois.xde <- function(pars) {
  FoIpar <- list()
  class(FoIpar) <- 'pois'
  pars$FoIpar <- FoIpar
  pars$FoI    <- list()
  return(pars)
}

#' @title Set up Poisson Exposure and Infection for `dts`
#' @description Set up the Poisson model for
#' exposure for discrete time systems
#' @param pars an **`xds`** object
#' @return an  **`xds`** object
#' @seealso Also, see [F_ar.pois]
#' @export
setup_exposure_pois.dts <- function(pars) {
  ARpar <- list()
  class(ARpar) <- 'pois'
  pars$ARpar <- ARpar
  pars$AR = list()
  return(pars)
}
