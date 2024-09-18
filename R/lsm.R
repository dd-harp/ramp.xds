
#' @title Set the LSM
#' @description Set the value of exogenous variables related to
#' LSM
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
LSM <- function(t, pars) {
  UseMethod("LSM", pars$lsm)
}

#' @title Set no LSM
#' @description The null model for LSM
#' @inheritParams LSM
#' @return [list]
#' @export
LSM.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no LSM"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_lsm <- function(pars) {
  LSM <- 'none'
  class(LSM) <- 'none'
  pars$lsm <- LSM
  return(pars)
}

#' @title Set no LSM
#' @description The null model for LSM
#' @inheritParams LSM
#' @return [list]
#' @export
LSM.dynamic <- function(t, pars) {
  pars <- TreatHabitats(t, pars)
  pars <- LSMEffects(t, pars)
  pars <- LSMCoverage(t, pars)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param treat_habitats_name the name of a model for mass LSM distribution
#' @param treat_habitats_opts options for the LSM distribution model
#' @param effects_name the name of a model for LSM effects
#' @param effects_opts options for the LSM effects model
#' @param coverage_name the name of a model for LSM coverage
#' @param coverage_opts options for the LSM coverage model
#' @param effectsizes_name the name of a model for LSM effect sizes
#' @param effectsizes_opts options for the LSM effect sizes model
#' @return an **`xds`** object
#' @export
xds_setup_lsm = function(pars,
                        treat_habitats_name = 'none', treat_habitats_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  pars = dynamic_vector_control(pars)
  LSM <- list()
  class(LSM) <- 'dynamic'
  pars$lsm <- LSM
  pars <- setup_treat_habitats(treat_habitats_name, pars, treat_habitats_opts)
  pars <- setup_lsm_effects(effects_name, pars, effects_opts)
  pars <- setup_lsm_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_lsm_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

