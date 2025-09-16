# The traps library is found in ramp.traps

#' @title Set up Mosquito traps 
#' 
#' @description Create a junction  
#' for mosquito traps 
#' 
#' @note Mosquito trap models are in
#' `ramp.control` 
#' 
#' @param t current simulation time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' @return an **`xds`** object
#' @export
Traps = function(t, y, xds_obj){
  UseMethod("Traps", xds_obj$variables$traps_obj)
}

#' @title Set the values of exogenous variables
#' @description After none setup, no exogenous
#' variables are configured so traps returns
#' the **`xds`** object without modification
#' 
#' @inheritParams Traps
#'
#' @return an **`xds`** object
#' @export
Traps.none = function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Setup the Junction for Exogenous traps
#' @description This sets up the `none` option
#' for exogenous traps: no traps.
#' @param xds_obj an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_traps_object = function(xds_obj){
  traps <- list() 
  class(traps) <- 'none'
  traps$name <- "Junction: Mosquito Traps" 
  traps$ports <- "Ports: Ovitraps, Blood Traps, Sugar Baited Traps" 
  xds_obj$ML_interface$Qtraps = list()
  xds_obj$ML_interface$Qtraps[[1]] <- rep(0, xds_obj$nPatches)
  xds_obj$XY_interface$Btraps = list()
  xds_obj$XY_interface$Btraps[[1]] <- rep(0, xds_obj$nPatches)
  xds_obj$variables$traps_obj <- traps
  return(xds_obj)
}