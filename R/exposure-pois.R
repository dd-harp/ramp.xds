
#' @title A Poisson model for Exposure
#' @description Compute the daily FoI, \eqn{h},
#' given the daily EIR, \eqn{E},
#' under a Poisson model for the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{h = b E}
#' @inheritParams F_foi
#' @return a [numeric] vector of length `nStrata`
#' @export
F_foi.pois <- function(eir, b, pars){
  b*eir
}

#' @title A Poisson model for Exposure
#' @description Compute the daily AR, \eqn{\alpha},
#' given the daily EIR, \eqn{E},
#' under a Poisson modelfor the distribution
#' of bites per person, and \eqn{b}, the probability of
#' infection per infective bite:
#' \deqn{\alpha = 1 - e^{- b E}}
#' @inheritParams F_ar
#' @return dEIR as a [numeric] vector of length \eqn{n_h=}`nStrata`
#' @export
F_ar.pois <- function(eir, b, pars){
  1 - exp(-b*eir)
}

#' @title Convert AR to EIR under a Poisson model for Exposure
#' @description The inverse of [F_ar.pois]
#' is \deqn{E = -\frac{\log(1-ar)}{b}}
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @export
ar2eir.pois <- function(ar, b, pars){
  -log(1-ar)/b
}

#' @title Convert FoI to EIR under a Poisson model for Exposure
#' @description The inverse of [F_foi.pois] is \eqn{E=h/b}
#' @inheritParams foi2eir
#' @return dEIR as a [numeric] vector of length \eqn{n_h=}`nStrata`
#' @export
foi2eir.pois <- function(foi, b, pars){
   foi/b
}

#' @title Set up the Poisson model of exposure
#' @param pars an **`xds`** object
#' @return the modified **`xds`** object
#' @export
setup_exposure_pois <- function(pars) {
  UseMethod("setup_exposure_pois", pars$xds)
}

#' @title Set up Poisson Exposure for `xde`
#' @description Set up the Poisson model for
#' exposure for continuous time models
#' @param pars an **`xds`** object
#' @return the modified **`xds`** object
#' @export
setup_exposure_pois.xde <- function(pars) {
  FoIpar <- list()
  class(FoIpar) <- 'pois'
  pars$FoIpar <- FoIpar
  pars$FoI    <- list()
  return(pars)
}

#' @title Set up Poisson Exposure for `dts`
#' @description Set up the Poisson model for
#' exposure for discrete time systems
#' @param pars an **`xds`** object
#' @return the modified **`xds`** object
#' @export
setup_exposure_pois.dts <- function(pars) {
  ARpar <- list()
  class(ARpar) <- 'pois'
  pars$ARpar <- ARpar
  pars$AR = list()
  return(pars)
}
