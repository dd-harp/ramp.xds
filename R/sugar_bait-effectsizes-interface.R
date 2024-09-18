#' @title Compute sugar bait effect sizes
#' @description Set the effect sizes of sugar baits
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SugarBaitEffectSizes <- function(t, pars) {
  UseMethod("SugarBaitEffectSizes", pars$sugar_baits$effectsizes)
}

#' @title Set up sugar baits effect sizes
#' @description
#' Set up sugar baits effect sizes
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_sugar_bait_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_sugar_bait_effectsizes", name)
}

