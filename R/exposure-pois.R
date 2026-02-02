
#' @title Set up a Poisson model for Exposure and Infection
#' 
#' @inheritParams setup_exposure
#' 
#' @return an  **`xds`** object
#' @export
#' @keywords internal
#' @seealso Also, see [F_foi.pois] and [F_ar.pois]
#'
setup_exposure.pois <- function(EHname, xds_obj, i=1, options = list()) {
  xds_obj$XY_interface$env_het_obj[[i]] = make_exposure_pois()
  xds_obj$terms$FoI = list()
  xds_obj$terms$FoI[[1]] = list()
  xds_obj$terms$AR = list()
  xds_obj$terms$AR[[1]] = list()
  return(xds_obj)
}

#' @title Make a Poisson Exposure Model Object 
#' 
#' @description Set up the Poisson model for
#' exposure for continuous time models
#' 
#' @return a local exposure model object
#' 
#' @seealso Also, see [F_foi.pois]
#' @export
make_exposure_pois <- function() {
  local_exposure_object <- list()
  class(local_exposure_object) <- 'pois'
  return(local_exposure_object)
}

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
#' @keywords internal
#' @export
F_foi.pois <- function(eir, b, env_het_obj){
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
#' @keywords internal
#' @export
F_ar.pois <- function(eir, b, env_het_obj){
  1 - exp(-b*eir)
}

#' @title Convert AR to EIR under a Poisson model for Exposure
#' @description The inverse of [F_ar.pois]
#' is \deqn{E = -\frac{\log(1-ar)}{b}}
#' @inheritParams ar2eir
#' @return a [numeric] vector of length `nStrata`
#' @seealso Also, see [F_ar.pois]
#' @keywords internal
#' @export
ar2eir.pois <- function(ar, b, env_het_obj){
  -log(1-ar)/b
}

#' @title Convert FoI to EIR under a Poisson model for Exposure
#' @description The inverse of [F_foi.pois] is \eqn{E=h/b}
#' @inheritParams foi2eir
#' @return dEIR as a [numeric] vector of length \eqn{n_h=}`nStrata`
#' @seealso Also, see [F_foi.pois]
#' @keywords internal
#' @export
foi2eir.pois <- function(foi, b, env_het_obj){
   foi/b
}

