
#' @title Set the temperature
#' @description Set the value of exogenous variables related to
#' temperature
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Temperature <- function(t, pars) {
  UseMethod("Temperature", pars$forcing$weather$temperature)
}

#' @title Set no temperature
#' @description The null model for temperature
#' @inheritParams Temperature
#' @return [list]
#' @export
Temperature.basic <- function(t, pars) {
  return(pars)
}

#' @title Set up "no temperature"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_temperature <- function(pars) {
  temperature <- 'basic'
  class(temperature) <- 'basic'
  pars$forcing$weather$temperature <- temperature
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param Tname the name of a model to set up
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_temperature = function(Tname, pars, Topts=list()){
  class(Tname) <- Tname
  UseMethod("setup_temperature", Tname)
}

#' @title Set no temperature
#' @description The null model for temperature
#' @inheritParams Temperature
#' @return [list]
#' @export
Temperature.func <- function(t, pars) {with(pars$temperature,{
  pars$vars$Temperature = meanT*F_season(t)*F_trend(t)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_temperature
#' @export
setup_temperature.func = function(Tname, pars, Topts=list()){
  pars = setup_temperature_sin(pars, Topts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param Topts a list of options to override defaults
#' @param meanT the mean temperature
#' @param F_season the seasonal signal in temperature
#' @param F_trend a temporal trend in temperature
#' @return an **`xds`** object
#' @export
setup_temperature_func = function(pars, Topts=list(), meanT=30, F_season=F_flat, F_trend=F_flat){
   temperature <- list()
   class(temperature) <- 'func'
   temperature$meanT <- meanT
   temperature$F_season <- F_season
   temperature$F_trend <- F_trend
   pars$temperature <- temperature
   return(pars)
}

