# generic methods for mass distribution of drugs, vaccines, etc.

#' @title Methods for mass distributing health interventions
#' @description This method dispatches on the type of `pars$mass_health`.
#' @param t current simulation time
#' @param y state variables
#' @param pars a [list]
#' @return [list]
#' @export
MassHealth <- function(t, y, pars) {
  UseMethod("MassHealth", pars$mass_health)
}

#' @title Methods for distributing interventions during mass_healthal visits
#' @description Implements [MassHealth] for the none model (do nothing)
#' @inheritParams MassHealth
#' @return [list]
#' @export
MassHealth.none <- function(t, y, pars) {
  return(pars)
}

#' @title Set up the none model for mass_healthal distribution (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_mass_health  <- function(pars) {
  mass_health <- list()
  class(mass_health) <- 'none'
  pars$mass_health <- mass_health
  return(pars)
}
