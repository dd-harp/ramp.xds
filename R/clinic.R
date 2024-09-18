# generic methods for distributing interventions at clinics

#' @title Methods for distributing interventions during clinical visits
#' @description This method dispatches on the type of `pars$clinic`.
#' @param t current simulation time
#' @param y state variables
#' @param pars a [list]
#' @return [list]
#' @export
Clinic <- function(t, y, pars) {
  UseMethod("Clinic", pars$clinic)
}

#' @title Methods for distributing interventions during clinical visits
#' @description Implements [Clinic] for the none model (do nothing)
#' @inheritParams Clinic
#' @return [list]
#' @export
Clinic.none <- function(t, y, pars) {
  return(pars)
}

#' @title Set up the none model for clinical distribution (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_clinic  <- function(pars) {
  clinic <- list()
  class(clinic) <- 'none'
  pars$clinic <- clinic
  return(pars)
}
