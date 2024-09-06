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
  with(pars$TRAVEL,{
     travel_foi = delta*F_season(t)*F_trend(t)
     return(travel_foi)
  })
}

#' @title A model for the travel FoI
#' @description Implements [travel_malaria] through a model for the travel FoI
#' @inheritParams travel_malaria
#' @return a [numeric]
#' @export
travel_malaria.dts <- function(t, pars) {
  with(pars$TRAVEL,{
     travel_ar = delta*F_season(t)*F_trend(t)
     return(travel_ar)
  })
}

#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @param pars a [list]
#' @param time_traveling the time spent traveling
#' @param delta the travel FoI
#' @param F_season a function describing a seasonal pattern
#' @param F_trend a function describing a trend
#' @param i the host species index
#' @return a [list]
#' @export
setup_travel_static = function(pars, time_traveling=0, delta=0,
                               F_season=F_flat, F_trend=F_flat, i=1){
  UseMethod("setup_travel_static", pars$xds)
}

#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @inheritParams setup_travel_static
#' @return a [list]
#' @export
setup_travel_static.xde = function(pars, time_traveling=0, delta=0,
                                   F_season=F_flat, F_trend=F_flat, i=1){
  TRAVEL <- list()
  class(TRAVEL) <- 'static'
  pars$TRAVEL <- TRAVEL
  pars$time_traveling[[i]] = rep(time_traveling, pars$nStrata[i])
  pars$TRAVEL$delta = rep(delta, pars$nStrata[i])
  pars$TRAVEL$F_season = F_season
  pars$TRAVEL$F_trend = F_trend
  return(pars)
}

#' @title A function to set up malaria importation
#' @description Setup a static model for travel malaria
#' @inheritParams setup_travel_static
#' @return a [list]
#' @export
setup_travel_static.dts = function(pars,  time_traveling=0, delta=0,
                                   F_season = F_flat, F_trend=F_flat, i=1){
  TRAVEL <- list()
  class(TRAVEL) <- 'static'
  pars$time_traveling[[i]] = rep(time_traveling, pars$nStrata[i])
  pars$TRAVEL$delta = rep(delta, pars$nStrata[i])
  pars$TRAVEL$F_season = F_season
  pars$TRAVEL$F_trend = F_trend
  return(pars)
}
