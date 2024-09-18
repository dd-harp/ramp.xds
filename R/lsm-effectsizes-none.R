
#' @title Set no lsm_effectsizes
#' @description The null model for lsm_effectsizes
#' @inheritParams LSMEffectSizes
#' @return [list]
#' @export
LSMEffectSizes.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no lsm_effectsizes"
#' @inheritParams setup_lsm_effectsizes
#' @return an **`xds`** object
#' @export
setup_lsm_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- 'none'
  class(effectsizes) <- 'none'
  pars$lsm$effectsizes <- effectsizes
  return(pars)
}
