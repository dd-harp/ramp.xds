#' @title Set the area spray effectsizes
#' @description Set the value of exogenous variables related to
#' area spray effectsizes
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
AreaSprayEffectSizes <- function(t, pars, s) {
  UseMethod("AreaSprayEffectSizes", pars$area_spray$effectsizes)
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
setup_area_spray_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_effectsizes", name)
}

