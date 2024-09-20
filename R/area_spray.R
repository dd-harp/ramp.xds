
#' @title Set the AreaSpray
#' @description Set the value of exogenous variables related to
#' AreaSpray
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSpray <- function(t, pars) {
  UseMethod("AreaSpray", pars$AreaSpray)
}

#' @title Set no AreaSpray
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no AreaSpray"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_area_spray <- function(pars) {
  AreaSpray <- 'none'
  class(AreaSpray) <- 'none'
  pars$area_spray <- AreaSpray
  return(pars)
}

#' @title Set no AreaSpray
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.dynamic <- function(t, pars) {
  pars <- SprayArea(t, pars)
  pars <- AreaSprayEffects(t, pars)
  pars <- AreaSprayCoverage(t, pars)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param spray_area_name the name of a model for mass bed net distribution
#' @param spray_area_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
xds_setup_area_spray = function(pars,
                        spray_area_name = 'none', spray_area_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  pars = dynamic_vector_control(pars)
  AreaSprays <- list()
  class(AreaSprays) <- 'dynamic'
  pars <- setup_spray_area(spray_area_name, pars, spray_area_opts)
  pars <- setup_area_spray_effects(effects_name, pars, effects_opts)
  pars <- setup_area_spray_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_area_spray_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

