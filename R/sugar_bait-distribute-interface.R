
#' @title Set the distribute_sugar_baits_sugar_baits
#' @description Set the value of exogenous variables related to
#' distribute_sugar_baits_sugar_baits
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
DistributeSugarBaits <- function(t, pars) {
  UseMethod("DistributeSugarBaits", pars$sugar_baits$distribute)
}


#' @title Set up dynamic sugar_baits
#' @description If dynamic sugar_baits has not
#' already been set up, then turn on dynamic
#' sugar_baits and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_distribute_sugar_baits = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_distribute_sugar_baits", name)
}
