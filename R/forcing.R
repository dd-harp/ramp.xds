# The forcing library is found in ramp.forcing

#' @title Set the values of exogenous variables
#' @description Set the values of
#' exogenous variables.
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Forcing = function(t, pars){
  UseMethod("Forcing", pars$forcing)
}

#' @title Set the values of exogenous variables
#' @description After none setup, no exogenous
#' variables are configured so forcing returns
#' the **`xds`** object without modification
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Forcing.none = function(t, pars){
  return(pars)
}

#' @title none set up for exogenous forcing
#' @description This sets up the `none` option
#' for exogenous forcing: no forcing.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_forcing = function(pars){
  forcing <- 'none'
  class(forcing) <- 'none'
  pars$forcing <- forcing
  return(pars)
}

