
#' @title Set the values variables for health interventions
#' @description Set the values of
#' variables for health interventions.
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Health = function(t, y, pars){
  UseMethod("Health", pars$health)
}

#' @title Set no exogenous health variables
#' @description After none setup, no exogenous
#' variables are configured so `Health` returns
#' the **`xds`** object without modification
#' @inheritParams Health
#' @return an **`xds`** object
#' @export
Health.none = function(t, y, pars){
  return(pars)
}

#' @title none set up for exogenous health
#' @description This sets up the `none` option
#' for exogenous health: no health.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_health = function(pars){
  health <- 'none'
  class(health) <- 'none'
  pars$health <- health
  return(pars)
}



