

#' @title Set the use_bednets
#' @description Set the value of exogenous variables related to
#' use_bednets
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
UseBedNets <- function(t, pars) {
  UseMethod("UseBedNets", pars$bednets$use)
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
setup_use_bednets = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_use_bednets", name)
}
