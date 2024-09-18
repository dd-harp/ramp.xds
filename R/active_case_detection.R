# generic methods for mass distribution of drugs, vaccines, etc.

#' @title Methods for mass distributing health interventions
#' @description This method dispatches on the type of `pars$active_case_detection`.
#' @param t current simulation time
#' @param y state variables
#' @param pars a [list]
#' @return [list]
#' @export
ActiveCaseDetection <- function(t, y, pars) {
  UseMethod("ActiveCaseDetection", pars$active_case_detection)
}

#' @title Methods for distributing interventions during active_case_detectional visits
#' @description Implements [ActiveCaseDetection] for the none model (do nothing)
#' @inheritParams ActiveCaseDetection
#' @return [list]
#' @export
ActiveCaseDetection.none <- function(t, y, pars) {
  return(pars)
}

#' @title Set up the none model for active_case_detectional distribution (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_active_case_detection  <- function(pars) {
  active_case_detection <- list()
  class(active_case_detection) <- 'none'
  pars$active_case_detection <- active_case_detection
  return(pars)
}
