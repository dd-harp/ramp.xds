# generic methods for exogenous forcing by weather

#' @title Methods for exogenous variables describing weather
#' @description This method dispatches on the type of `pars$WEATHER`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
Weather <- function(t, pars) {
  UseMethod("Weather", pars$WEATHER)
}

#' @title Methods for exogenous variables describing weather
#' @description Implements [Weather] for the no_forcing model (no variables)
#' @inheritParams Weather
#' @return [list]
#' @export
Weather.no_forcing <- function(t, pars) {
  return(pars)
}

#' @title Set up the no_forcing model for weather
#' @param pars a [list]
#' @return [list]
#' @export
setup_weather_no_forcing <- function(pars) {
  WEATHER <- list()
  class(WEATHER) <- 'no_forcing'
  pars$WEATHER <- WEATHER
  return(pars)
}

#' @title Methods for exogenous variables describing weather
#' @description Implements exogenous forcing by [Weather]
#' @inheritParams Weather
#' @return [list]
#' @export
Weather.forced <- function(t, pars) {
  pars = Temperature(t, pars)
  pars = Rainfall(t, pars)
  pars = Relative_Humidity(t, pars)
  return(pars)
}

#' @title Set up the forced model for weather
#' @param pars a [list]
#' @return [list]
#' @export
setup_weather_forced <- function(pars) {
  WEATHER <- list()
  class(WEATHER) <- 'forced'
  pars$WEATHER <- WEATHER
  pars = setup_temperature_no_forcing(pars)
  pars = setup_rainfall_no_forcing(pars)
  pars = setup_relative_humidity_no_forcing(pars)
  return(pars)
}

#' @title Methods for exogenous variables describing temperature
#' @description This method dispatches on the type of `pars$TEMPERATURE`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
Temperature <- function(t, pars) {
  UseMethod("Temperature", pars$TEMPERATURE)
}

#' @title Methods for exogenous variables describing temperature
#' @description Implements [Temperature] for the no_forcing model (no variables)
#' @inheritParams Temperature
#' @return [list]
#' @export
Temperature.no_forcing <- function(t, pars) {
  return(pars)
}

#' @title Set up the no_forcing model for temperature
#' @param pars a [list]
#' @return [list]
#' @export
setup_temperature_no_forcing <- function(pars) {
  TEMPERATURE <- list()
  class(TEMPERATURE) <- 'no_forcing'
  pars$TEMPERATURE <- TEMPERATURE
  return(pars)
}

#' @title Set up the no_forcing model for RAINFALL
#' @param pars a [list]
#' @return [list]
#' @export
setup_rainfall_no_forcing <- function(pars) {
  RAINFALL <- list()
  class(RAINFALL) <- 'no_forcing'
  pars$RAINFALL <- RAINFALL
  return(pars)
}

#' @title Methods for exogenous variables describing rainfall
#' @description This method dispatches on the type of `pars$RAINFALL`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
Rainfall <- function(t, pars) {
  UseMethod("Rainfall", pars$RAINFALL)
}

#' @title Methods for exogenous variables describing rainfall
#' @description Implements [Rainfall] for the no_forcing model (no variables)
#' @inheritParams Rainfall
#' @return [list]
#' @export
Rainfall.no_forcing <- function(t, pars) {
  return(pars)
}

#' @title Set up the no_forcing model for HUMIDITY
#' @param pars a [list]
#' @return [list]
#' @export
setup_relative_humidity_no_forcing <- function(pars) {
  HUMIDITY <- list()
  class(HUMIDITY) <- 'no_forcing'
  pars$HUMIDITY <- HUMIDITY
  return(pars)
}

#' @title Methods for exogenous variables describing relative humidity
#' @description This method dispatches on the type of `pars$HUMIDITY`.
#' @param t current simulation time
#' @param pars a [list]
#' @return [list]
#' @export
Relative_Humidity <- function(t, pars) {
  UseMethod("Relative_Humidity", pars$HUMIDITY)
}

#' @title Methods for exogenous variables describing relative humidity
#' @description Implements [Relative_Humidity] for the no_forcing model (no variables)
#' @inheritParams Relative_Humidity
#' @return [list]
#' @export
Relative_Humidity.no_forcing <- function(t, pars) {
  return(pars)
}
