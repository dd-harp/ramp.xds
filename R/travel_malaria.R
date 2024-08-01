# generic methods for a model of travel malaria

#' @title Simulate travel malaria
#' @description This method dispatches on the type of `pars$TRAVEL`.
#' @param t current simulation time
#' @param pars a [list]
#' @return the travel FoI, a [numeric] value
#' @export
travel_malaria <- function(t, pars) {
  UseMethod("travel_malaria", pars$xds)
}

#' @title A model for the travel FoI
#' @description Implements [travel_malaria] through a model for the travel FoI
#' @inheritParams travel_malaria
#' @return a [numeric]
#' @export
travel_malaria.xde <- function(t, pars) {
    return(pars$TRAVEL$delta)
}

#' @title A model for the travel FoI
#' @description Implements [travel_malaria] through a model for the travel FoI
#' @inheritParams travel_malaria
#' @return a [numeric]
#' @export
travel_malaria.dts <- function(t, pars) {
    return(pars$TRAVEL$delta)
}

#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @param pars a [list]
#' @param delta the travel FoI
#' @return a [list]
#' @export
setup_travel_static = function(pars, delta=0){
  UseMethod("setup_travel_static", pars$xds)
}

#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @inheritParams setup_travel_static
#' @return a [list]
#' @export
setup_travel_static.xde = function(pars, delta=0){
  TRAVEL <- list()
  class(TRAVEL) <- 'static'
  pars$TRAVEL <- TRAVEL
  pars$TRAVEL$delta = delta
  return(pars)
}

#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @inheritParams setup_travel_static
#' @return a [list]
#' @export
setup_travel_static.dts = function(pars, delta=0){
  TRAVEL <- list()
  class(TRAVEL) <- 'static'
  pars$TRAVEL <- TRAVEL
  pars$delta = delta
  return(pars)
}
