# generic methods for distributing interventions at schools

#' @title Methods for distributing interventions during schoolal visits
#' @description This method dispatches on the type of `pars$school`.
#' @param t current simulation time
#' @param y state variables
#' @param pars a [list]
#' @return [list]
#' @export
School <- function(t, y, pars) {
  UseMethod("School", pars$school)
}

#' @title Methods for distributing interventions during schoolal visits
#' @description Implements [School] for the none model (do nothing)
#' @inheritParams School
#' @return [list]
#' @export
School.none <- function(t, y, pars) {
  return(pars)
}

#' @title Set up the none model for schoolal distribution (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_school  <- function(pars) {
  school <- list()
  class(school) <- 'none'
  pars$school <- school
  return(pars)
}
