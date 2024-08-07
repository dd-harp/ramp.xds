
#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Forcing = function(t, pars){
  UseMethod("Forcing", pars$forcing)
}

#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Forcing.static = function(t, pars){
  return(pars)
}

#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Forcing.setup = function(t, pars){
  class(pars$forcing) <- 'dynamic'
  pars <- Forcing(t, pars)
  class(pars$forcing) <- 'static'
  return(pars)
}

#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
Forcing.dynamic = function(t, pars){
  pars <- Weather(t, pars)
  pars <- Hydrology(t, pars)
  pars <- Shock(t, pars)
  pars <- Development(t, pars)
  pars <- Resources(t, pars)
  return(pars)
}

#' @title Set up exogenous forcing
#' @description This method...
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_forcing = function(pars){
  pars <- setup_weather_no_forcing(pars)
  pars <- setup_hydrology_no_forcing(pars)
  pars <- setup_no_shock(pars)
  pars <- setup_no_development(pars)
  pars <- setup_resources_static(pars)
  return(pars)
}
