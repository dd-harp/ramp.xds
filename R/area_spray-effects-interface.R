
#' @title Set the area spray effects area spray
#' @description Set the value of exogenous variables related to
#' area spraying 
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSprayEffects <- function(t, pars) {
  UseMethod("AreaSprayEffects", pars$area_spray$effects)
}


#' @title Set up dynamic area spray
#' @description If dynamic area spray has not
#' already been set up, then turn on dynamic
#' area spray and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_area_spray_effects = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_effects", name)
}
