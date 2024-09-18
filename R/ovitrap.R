# generic methods for oviposition traps

#' @title Methods for oviposition traps
#' @description This method dispatches on the type of `pars$ovitraps`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
OviTraps <- function(t, pars) {
  UseMethod("OviTraps", pars$ovitraps)
}

#' @title Methods for oviposition traps
#' @description Implements [OviTraps] for the none model (do nothing)
#' @inheritParams OviTraps
#' @return [list]
#' @export
OviTraps.none <- function(t, pars) {
  return(pars)
}

#' @title Set up the none model for oviposition traps (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_no_ovitraps <- function(pars) {
  ovitraps <- list()
  class(ovitraps) <- 'none'
  pars$ovitraps <- ovitraps
  return(pars)
}
