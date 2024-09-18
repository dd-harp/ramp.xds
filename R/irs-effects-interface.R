
#' @title Set the irs_effects_irs
#' @description Set the value of exogenous variables related to
#' irs_effects_irs
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
IRSEffects <- function(t, pars) {
  UseMethod("IRSEffects", pars$irs$effects)
}


#' @title Set up dynamic irs
#' @description If dynamic irs has not
#' already been set up, then turn on dynamic
#' irs and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_irs_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_irs_effects", name)
}
