
#' @title Set the sugar bait coverage
#' @description Set the coverage levels of
#' sugar baits
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SugarBaitCoverage <- function(t, pars) {
  UseMethod("SugarBaitCoverage", pars$sugar_baits$coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the module name
#' @param pars an **`xds`** object
#' @param opts a list of options to override the defaults
#' @return an **`xds`** object
#' @export
setup_sugar_bait_coverage = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_sugar_bait_coverage", name)
}


