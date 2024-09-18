
#' @title No sugar bait effect sizes
#' @description The no sugar bait model for sugar bait effect sizes
#' @inheritParams SugarBaitEffectSizes
#' @return [list]
#' @export
SugarBaitEffectSizes.none <- function(t, pars) {
  return(pars)
}

#' @title Set up sugar bait effect sizes for no sugar baits
#' @inheritParams setup_sugar_bait_effectsizes
#' @return an **`xds`** object
#' @export
setup_sugar_bait_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- list()
  class(effectsizes) <- 'none'
  effectsizes$name <- 'none'
  pars$sugar_baits$effectsizes <- effectsizes
  return(pars)
}
