#' @title Set no lsm_effects
#' @description The null model for lsm_effects
#' @inheritParams LSMEffects
#' @return an **`xds`** object
#' @export
LSMEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no lsm_effects"
#' @inheritParams setup_lsm_effects
#' @return an **`xds`** object
#' @export
setup_lsm_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$lsm$effects <- effects
  return(pars)
}
