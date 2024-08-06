
#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param y vector of state variables
#' @param pars a [list]
#' @return none
#' @export
Forcing = function(t, y, pars){
  UseMethod("Forcing", pars$forcing)
}

#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param y vector of state variables
#' @param pars a [list]
#' @return none
#' @export
Forcing.static = function(t, y, pars){
  return(pars)
}

#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param y vector of state variables
#' @param pars a [list]
#' @return none
#' @export
Forcing.setup = function(t, y, pars){
  class(pars$forcing) <- 'dynamic'
  pars <- Forcing(t, y, pars)
  class(pars$forcing) <- 'static'
  return(pars)
}

#' @title Set the values of exogenous variables
#' @description This method...
#' @param t current simulation time
#' @param y vector of state variables
#' @param pars a [list]
#' @return none
#' @export
Forcing.dynamic = function(t, y, pars){
  pars <- Weather(t, pars)
  pars <- Hydrology(t, pars)
  pars <- Shock(t, pars)
  pars <- Development(t, pars)
  pars <- Visiting(t, pars)
  pars <- Resources(t, pars)
  # Control
  pars <- Control(t, y, pars)
  pars <- Behavior(t, y, pars)
  pars <- VectorControlEffects(t, y, pars)
  return(pars)
}

#' @title Set up exogenous forcing
#' @description This method...
#' @param pars an `xds` object
#' @return the modified `xds` object
#' @export
setup_forcing = function(pars){
  pars <- setup_weather_no_forcing(pars)
  pars <- setup_hydrology_no_forcing(pars)
  pars <- setup_no_shock(pars)
  pars <- setup_no_development(pars)
  pars <- setup_control_no_control(pars)
  pars <- setup_behavior_no_behavior(pars)
  pars <- setup_habitat_dynamics_static(pars)
  pars <- setup_resources_static(pars)
  return(pars)
}
