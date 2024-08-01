# human and mosquito behaviors

#' @title Methods for dynamic human and mosquito behaviors
#' @description This method dispatches on the type of `pars$BEHAVIOR`.
#' @param t current simulation time
#' @param y state variables
#' @param pars a [list]
#' @return [list]
#' @export
Behavior <- function(t, y, pars) {
  UseMethod("Behavior", pars$BEHAVIOR)
}

#' @title Methods for dynamic human and mosquito behaviors
#' @description Implements [Behavior] for the no_behavior model (no changes)
#' @inheritParams Behavior
#' @return [list]
#' @export
Behavior.no_behavior <- function(t, y, pars) {
 return(pars)
}

#' @title Make parameters for the no_behavior model for resource availability (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_behavior_no_behavior<- function(pars) {
  BEHAVIOR <- list()
  class(BEHAVIOR) <- 'no_behavior'
  pars$BEHAVIOR <- BEHAVIOR
  return(pars)
}

#' @title Setup behavior
#' @param pars a [list]
#' @return [list]
#' @export
setup_behavior <- function(pars) {
  UseMethod("setup_behavior", pars$BEHAVIOR)
}

#' @title Setup behavior
#' @param pars a [list]
#' @return [list]
#' @export
setup_behavior.no_behavior <- function(pars) {
  setup_behavior_forced(pars)
}

#' @title Setup behavior
#' @param pars a [list]
#' @return [list]
#' @export
setup_behavior.forced<- function(pars) {pars}

#' @title Methods for dynamic human and mosquito behaviors
#' @description Implements [Behavior] for the forced model (no changes)
#' @inheritParams Behavior
#' @return [list]
#' @export
Behavior.forced <- function(t, y, pars) {
  pars = UseBedNet(t, y, pars)
  pars = CareSeeking(t, y, pars)
#  pars = Protect(t, y, pars)
#  pars = Mobility(t, y, pars)
#  pars = MozySearch(t, y, pars)
 return(pars)
}

#' @title Make parameters for the forced model for resource availability (do nothing)
#' @param pars a [list]
#' @return [list]
#' @export
setup_behavior_forced<- function(pars) {
  BEHAVIOR <- list()
  class(BEHAVIOR) <- 'forced'
  pars$BEHAVIOR <- BEHAVIOR
  pars = setup_care_seeking_no_behavior(pars)
#  pars = setup_protect_no_behavior(t, y, pars)
#  pars = setup_mobility_no_behavior(t, y, pars)
#  pars = setup_mozy_search_no_behavior(t, y, pars)
  return(pars)
}
