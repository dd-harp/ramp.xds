# generic methods for shocks

#' @title Set up shocks
#' @description This method dispatches on the type of `pars$SHOCK`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
Shock <- function(t, pars) {
  UseMethod("Shock", pars$SHOCK)
}

#' @title Set up shocks
#' @description Implements [Shock] for the no_shock model (do nothing)
#' @inheritParams Shock
#' @return [list]
#' @export
Shock.no_shock <- function(t, pars) {
  return(pars)
}

#' @title Set up the no_shock model for shocks (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_shock <- function(pars) {
  SHOCK <- list()
  class(SHOCK) <- 'no_shock'
  pars$SHOCK <- SHOCK
  return(pars)
}
