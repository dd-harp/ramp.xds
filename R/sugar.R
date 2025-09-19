# The sugar library is found in ramp.sugar

#' @title Set up Mosquito sugar 
#' 
#' @description Create a junction  
#' for mosquito sugar 
#' 
#' @note Mosquito trap models are in
#' `ramp.control` 
#' 
#' @param t current simulation time
#' @param y dependent variables vector
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
Sugar = function(t, y, xds_obj){
  UseMethod("Sugar", xds_obj$resources_obj$sugar_obj)
}

#' @title Set the values of exogenous variables
#' @description After none setup, no exogenous
#' variables are configured so sugar returns
#' the **`xds`** object without modification
#' 
#' @inheritParams Sugar
#' 
#' @return an **`xds`** object
#' @export
Sugar.none = function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Setup the Junction for Exogenous sugar
#' @description This sets up the `none` option
#' for exogenous sugar: no sugar.
#' @param xds_obj an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_sugar_object = function(xds_obj){
  sugar <- list() 
  class(sugar) <- 'none'
  sugar$name <- "Junction: Natural Sugar Dynamics" 
  sugar$ports <- "Sugar Availability, Sugar Baits" 
  xds_obj$resources_obj$sugar_obj <- sugar
  return(xds_obj)
}