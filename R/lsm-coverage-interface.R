
#' @title Set the lsm_coverage
#' @description Set the value of exogenous variables related to
#' lsm_coverage
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
LSMCoverage <- function(t, pars) {
  UseMethod("LSMCoverage", pars$lsm$coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_lsm_coverage = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_lsm_coverage", name)
}


