
#' @title Set the area spray coverage
#' @description Set the value of exogenous variables related to
#' area spray coverage
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
AreaSprayCoverage <- function(t, pars) {
  UseMethod("AreaSprayCoverage", pars$area_spray$coverage)
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
setup_area_spray_coverage = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_area_spray_coverage", name)
}


