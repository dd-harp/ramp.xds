
#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
BedNet <- function(t, pars) {
  UseMethod("BedNet", pars$bednets)
}

#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no bednet"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_bednets <- function(pars) {
  bednets <- list()
  bednets$name <- 'none'
  class(bednets) <- 'none'
  pars$bednets <- bednets
  pars$bednets$effectsizes <- bednets
  return(pars)
}

#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.dynamic <- function(t, pars) {
   pars <- DistributeBedNets(t, pars)
   pars <- OwnBedNets(t, pars)
   pars <- UseBedNets(t, pars)
   pars <- BedNetEffects(t, pars)
   pars <- BedNetCoverage(t, pars)
   return(pars)

}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param distribute_name the name of a model for mass bed net distribution
#' @param distribute_opts options for the bed net distribution model
#' @param own_name the name of a model for bed net ownership
#' @param own_opts options for the bed net ownership model
#' @param use_name the name of a model for bed net usage
#' @param use_opts options for the bed net usage model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effectsizes_name the name of a model for bed net effect sizes
#' @param effectsizes_opts options for the bed net effect sizes model
#' @return an **`xds`** object
#' @export
xds_setup_bednets = function(pars,
                        distribute_name = 'none', distribute_opts = list(),
                        own_name = 'none', own_opts = list(),
                        use_name = 'none', use_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effectsizes_name = 'none', effectsizes_opts = list()){

  pars <- dynamic_vector_control(pars)
  bednets <- list()
  bednets$name <- 'dynamic'
  class(bednets) <- 'dynamic'
  pars$bednets <- bednets


  pars <- setup_distribute_bednets(distribute_name, pars, distribute_opts)
  pars <- setup_own_bednets(own_name, pars, own_opts)
  pars <- setup_use_bednets(use_name, pars, use_opts)
  pars <- setup_bednet_effects(effects_name, pars, effects_opts)
  pars <- setup_bednet_coverage(coverage_name, pars, coverage_opts)
  pars <- setup_bednet_effectsizes(effectsizes_name, pars, effectsizes_opts)
  return(pars)
}

