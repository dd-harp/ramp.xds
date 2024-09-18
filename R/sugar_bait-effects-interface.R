
#' @title Compute sugar bait effects
#' @description Compute the effects of a sugar bait
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SugarBaitEffects <- function(t, pars) {
  UseMethod("SugarBaitEffects", pars$sugar_baits$effects)
}


#' @title Set up a sugar bait effects model
#' @description Set up a sugar bait effects model
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_sugar_bait_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_sugar_bait_effects", name)
}
