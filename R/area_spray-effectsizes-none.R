
#' @title Set no area_spray_effectsizes
#' @description The null model for area_spray_effectsizes
#' @inheritParams AreaSprayEffectSizes
#' @return [list]
#' @export
AreaSprayEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up no area spray effectsizes
#' @inheritParams setup_area_spray_effectsizes
#' @return an **`xds`** object
#' @export
setup_area_spray_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- 'none'
  class(effectsizes) <- 'none'
  pars$area_spray$effectsizes <- effectsizes
  return(pars)
}
