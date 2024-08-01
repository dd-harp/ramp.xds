# generic methods for mass distribution of medical interventions

#' @title Methods for mass medical interventions
#' @description This method dispatches on the type of `pars$MASS_MED`.
#' @param t current simulation time
#' @param y state variables
#' @param pars a [list]
#' @return [list]
#' @export
MassMedical <- function(t, y, pars) {
  UseMethod("MassMedical", pars$MASS_MED)
}

#' @title Methods for mass medical
#' @description Implements [MassMedical] for the no_control model (do nothing)
#' @inheritParams MassMedical
#' @return [list]
#' @export
MassMedical.no_control <- function(t, y, pars) {
  return(pars)
}

#' @title Set up the no_control model for mass medical (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_mass_medical_no_control <- function(pars) {
  MASS_MED <- list()
  class(MASS_MED) <- 'no_control'
  pars$MASS_MED <- MASS_MED
  return(pars)
}

#' @title Methods for mass medical
#' @description Implements [MassMedical]
#' @inheritParams MassMedical
#' @return [list]
#' @export
MassMedical.forced <- function(t, y, pars) {
  return(pars)
}

#' @title Set up a model for mass medical
#' @param pars a [list]
#' @return [list]
#' @export
setup_mass_medical_forced <- function(pars) {
  pars = setup_control(pars)
  MASS_MED <- list()
  class(MASS_MED) <- 'forced'
  pars$MASS_MED <- MASS_MED
#  pars = setup_mass_treat_no_control(pars)
#  pars = setup_vaccinate_no_control(pars)
#  pars = setup_mAb_no_control(pars)
  return(pars)
}
