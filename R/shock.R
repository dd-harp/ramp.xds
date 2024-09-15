
#' @title Set the shock
#' @description Set the value of exogenous variables related to
#' shock
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Schock <- function(t, pars) {
  UseMethod("Schock", pars$forcing$shock)
}

#' @title Set no shock
#' @description The null model for shock
#' @inheritParams Schock
#' @return [list]
#' @export
Schock.basic <- function(t, pars) {
  return(pars)
}

#' @title Set up "no shock"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_shock <- function(pars) {
  shock <- 'basic'
  class(shock) <- 'basic'
  pars$forcing$shock <- shock
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_shock = function(name, pars, Topts=list()){
  class(name) <- name
  UseMethod("setup_shock", name)
}

#' @title Set no shock
#' @description The null model for shock
#' @inheritParams Schock
#' @return [list]
#' @export
Schock.func <- function(t, pars) {
  pars = F_shock(t, pars)

}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_shock
#' @export
setup_shock.func = function(name="func", pars, Topts=list()){
  pars <- dynamic_forcing(pars)
  pars = setup_shock_func(pars, Topts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @param eventT the time when a schock occurs
#' @param F_schock the effects of the schock
#' @return an **`xds`** object
#' @export
setup_shock_func = function(pars, Topts=list(), eventT=365, F_schock=NULL){
  shock <- list()
  class(shock) <- 'func'
  shock$eventT = eventT
  shock$F_shock = F_shock
  return(pars)
}

