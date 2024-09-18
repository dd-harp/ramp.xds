
#' @title Set the bednet_effects
#' @description Set the value of exogenous variables related to
#' bednet_effects
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
BedNetEffects <- function(t, pars) {
  UseMethod("BedNetEffects", pars$bednets$effects)
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
setup_bednet_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_effects", name)
}


