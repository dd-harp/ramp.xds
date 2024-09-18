
#' @title Implement IRS
#' @description Set the value of exogenous variables related to
#' IRS
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
IRS <- function(t, pars) {
  UseMethod("IRS", pars$irs)
}

#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return an **`xds`** object
#' @export
IRS.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no irs"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_irs <- function(pars) {
  irs <- list()
  class(irs) = 'none'
  irs$name = 'none'
  pars$irs <- irs
  pars <- setup_irs_effectsizes("none", pars, list())
  return(pars)
}

#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return [list]
#' @export
IRS.dynamic <- function(t, pars) {
  pars <- SprayHouses(t, pars)
  pars <- IRSEffects(t, pars)
  pars <- IRSCoverage(t, pars)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param spray_houses_name the name of a model for mass bed net distribution
#' @param spray_houses_opts options for the bed net distribution model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
xds_setup_irs = function(pars,
                        spray_houses_name = 'none', spray_houses_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){
  pars = dynamic_vector_control(pars)
  irss <- list()
  class(irss) <- 'dynamic'
  pars <- setup_spray_houses(spray_houses_name, pars, spray_houses_opts)
  pars <- setup_irs_effects(effects_name, pars, effects_opts)
  pars <- setup_irs_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_irs_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

