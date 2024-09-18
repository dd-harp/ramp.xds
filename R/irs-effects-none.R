#' @title Set no irs_effects
#' @description The null model for irs_effects
#' @inheritParams IRSEffects
#' @return an **`xds`** object
#' @export
IRSEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no irs_effects"
#' @inheritParams setup_irs_effects
#' @return an **`xds`** object
#' @export
setup_irs_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$irs$effects <- effects
  return(pars)
}
