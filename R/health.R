# Functions to setup malaria control are found in ramp.control

#' @title Set the values variables for health-based malaria control  
#' @description Set the values of
#' variables for health interventions.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
Health = function(t, y, xds_obj){
  UseMethod("Health", xds_obj$health)
}

#' @title Set no exogenous health variables
#' @description After none setup, no exogenous
#' variables are configured so `Health` returns
#' the **`ramp.xds`** model object without modification
#' @inheritParams Health
#' @return a **`ramp.xds`** model object
#' @export
Health.none = function(t, y, xds_obj){
  return(xds_obj)
}

#' @title none set up for exogenous health
#' @description This sets up the `none` option
#' for exogenous health: no health.
#' @param xds_obj an **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_health = function(xds_obj){
  health <- 'none'
  class(health) <- 'none'
  xds_obj$health <- health
  return(xds_obj)
}



