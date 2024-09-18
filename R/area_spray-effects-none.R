#' @title Set no area spray effects
#' @description The null model for area spray effects
#' @inheritParams AreaSprayEffects
#' @return an **`xds`** object
#' @export
AreaSprayEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no area spray effects
#' @inheritParams setup_area_spray_effects
#' @return an **`xds`** object
#' @export
setup_area_spray_effects.none <- function(name, pars, opts) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$area_spray$effects <- effects
  return(pars)
}
