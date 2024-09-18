
#' @title Set no bednet_effects
#' @description The null model for bednet_effects
#' @inheritParams BedNetEffects
#' @return [list]
#' @export
BedNetEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no bednet_effects"
#' @inheritParams setup_bednet_effects
#' @return an **`xds`** object
#' @export
setup_bednet_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$bednets$effects <- effects
  return(pars)
}
