
#' @title Set the lsm_effects_lsm
#' @description Set the value of exogenous variables related to
#' lsm_effects_lsm
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
LSMEffects <- function(t, pars) {
  UseMethod("LSMEffects", pars$lsm$effects)
}


#' @title Set up dynamic lsm
#' @description If dynamic lsm has not
#' already been set up, then turn on dynamic
#' lsm and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_lsm_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_lsm_effects", name)
}
