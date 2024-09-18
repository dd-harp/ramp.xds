
#' @title Set no bednet_effectsizes
#' @description The null model for bednet_effectsizes
#' @inheritParams BedNetEffectSizes
#' @return [list]
#' @export
BedNetEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up "no bednet_effectsizes"
#' @inheritParams setup_bednet_effectsizes
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- 'none'
  class(effectsizes) <- 'none'
  pars$bednets$effectsizes <- effectsizes
  return(pars)
}
