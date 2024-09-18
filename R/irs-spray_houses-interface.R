
#' @title Set the spray_houses_irs
#' @description Set the value of exogenous variables related to
#' spray_houses_irs
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
SprayHouses <- function(t, pars) {
  UseMethod("SprayHouses", pars$irs$spray_houses)
}


#' @title Set up dynamic irs
#' @description If dynamic irs has not
#' already been set up, then turn on dynamic
#' irs and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_spray_houses = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_spray_houses", name)
}
