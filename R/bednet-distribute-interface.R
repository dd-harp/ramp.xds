
#' @title Set the distribute_bednets
#' @description Set the value of exogenous variables related to
#' distribute_bednets
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
DistributeBedNets <- function(t, pars) {
  UseMethod("DistributeBedNets", pars$bednets$distribute)
}


#' @title Set up dynamic bednets
#' @description If dynamic bednets has not
#' already been set up, then turn on dynamic
#' bednets and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_distribute_bednets = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_distribute_bednets", name)
}

