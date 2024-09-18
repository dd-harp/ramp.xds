
#' @title System Shocks
#' @description Set the value of exogenous variables related to
#' systemic shocks
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Shock <- function(t, pars) {
  UseMethod("Shock", pars$forcing$shock)
}

#' @title Set no shock
#' @description The null model for shock
#' @inheritParams Shock
#' @return [list]
#' @export
Shock.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no shock"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_shock <- function(pars) {
  shock <- 'none'
  class(shock) <- 'none'
  pars$forcing$shock <- shock
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_shock = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_shock", name)
}

#' @title Set no shock
#' @description The null model for shock
#' @inheritParams Shock
#' @return [list]
#' @export
Shock.func <- function(t, pars) {
  #pars = F_shock(t, pars)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_shock
#' @export
setup_shock.func = function(name="func", pars, opts=list()){
  pars <- dynamic_forcing(pars)
  pars = setup_shock_func(pars, opts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param eventT the time when a shock occurs
#' @param F_shock the effects of the shock
#' @return an **`xds`** object
#' @export
setup_shock_func = function(pars, opts=list(), eventT=365, F_shock=NULL){
  shock <- list()
  class(shock) <- 'func'
  shock$eventT = eventT
  shock$F_shock = F_shock
  return(pars)
}

