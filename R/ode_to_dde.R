
#' @title Compute as DDE 
#' 
#' @description Setup a differential equation model 
#' to be solved using `deSolve:dede` 
#' 
#' @details
#' For differential equations, the value of `xds_obj$xde` 
#' is set to `ode` by default. This utility gets called by delay
#' differential equation modules to change 
#' `xds_obj$xde` from `ode` to `dde` 
#'  
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' 
#' @export
ode_to_dde = function(xds_obj){
  UseMethod("ode_to_dde", xds_obj$xde)
}

#' @title Compute as DDE
#' @description If `class(xds_obj$xde) == "dde"` don't change anything
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
ode_to_dde.dde = function(xds_obj){
  return(xds_obj)
}

#' @title Compute as DDE
#' @description If `class(xds_obj$xde) == "dts"` don't change anything
#' @param xds_obj a **`ramp.xds`** modelobject
#' @return a **`ramp.xds`** model object
#' @export
ode_to_dde.dts = function(xds_obj){
  return(xds_obj)
}

#' @title Compute as DDE
#' @description If `class(xds_obj$xde) == "ode"` change it to `dde` 
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
ode_to_dde.ode = function(xds_obj){
  xds_obj$xde = 'dde'
  class(xds_obj$xde) ='dde'
  return(xds_obj)
}