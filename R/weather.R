# generic methods for exogenous forcing by weather

#' @title Set the values of exogenous variables describing weather
#' @description This method dispatches on the type of `pars$weather`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
Weather <- function(t, pars) {
  UseMethod("Weather", pars$weather)
}

#' @title Methods for exogenous variables describing weather
#' @description Implements a null weather model
#' @inheritParams Weather
#' @return [list]
Weather.basic <- function(t, pars) {
  return(pars)
}

#' @title Set up the no_forcing model for weather
#' @param pars a [list]
#' @return [list]
setup_no_weather <- function(pars) {
  weather <- 'basic'
  class(weather) <- 'basic'
  pars$forcing$weather <- weather
  return(pars)
}

#' @title Set up dynamic weather
#' @description If dynamic weather has not
#' already been set up, then turn on dynamic
#' weather and set up all its null models
#' @param pars an **`xds`** object
#' @return an **`xds`** object
dynamic_weather = function(pars){
  UseMethod("dynamic_weather", pars$weather)
}

#' @title Set up dynamic weather
#' @description If dynamic weather has not
#' already been set up, then turn on dynamic
#' weather and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
dynamic_weather.basic = function(pars){
  # turn on dynamic forcing
  pars <- dynamic_forcing(pars)
  weather <- 'dynamic'
  class(weather) <- 'dynamic'
  pars$weather <- weather
  pars <- setup_no_temperature(pars)
  pars <- setup_no_rainfall(pars)
  pars <- setup_no_humidity(pars)
  return(pars)
}

#' @title Set up dynamic weather
#' @description If dynamic weather has not
#' already been set up, then turn on dynamic
#' weather and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
dynamic_weather.setup = function(pars){
  return(pars)
}

#' @title Set up dynamic weather
#' @description If dynamic weather has not
#' already been set up, then turn on dynamic
#' weather and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
dynamic_weather.dynamic = function(pars){
  return(pars)
}

#' @title Methods for exogenous variables describing weather
#' @description Implements exogenous forcing by [Weather]
#' @inheritParams Weather
#' @return [list]
Weather.dynamic <- function(t, pars) {
  pars = Temperature(t, pars)
  pars = Rainfall(t, pars)
  pars = Humidity(t, pars)
  return(pars)
}

