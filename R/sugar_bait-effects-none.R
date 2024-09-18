#' @title Set up no sugar bait effects
#' @description Set up no sugar bait effects
#' @inheritParams SugarBaitEffects
#' @return an **`xds`** object
#' @export
SugarBaitEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no sugar bait effects
#' @inheritParams setup_sugar_bait_effects
#' @return an **`xds`** object
#' @export
setup_sugar_bait_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$sugar_baits$effects <- effects
  return(pars)
}
