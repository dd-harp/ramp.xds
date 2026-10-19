#' @title Setup the Habitat Interface for Egg Laying and Emergence
#'
#' @description Set up a part of the `xds` object that defines the interface for egg laying
#' @details
#' This implements a model for egg laying described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in **`ramp.xds`** requires a rigid interface
#' to guarantee mathematical consistency for egg laying and emergence.
#' The interface is defined by an object called `egg_laying` that is
#' attached to the `xds` object `xds_obj` as `xds_obj$egg_laying`.
#' The interface includes
#' - a habitat membership matrix, \eqn{N} made by [make_habitat_matrix]
#' - the habitat search weights
#' - a quantity that is motivated by mosquito searching for resources, called
#' habitat availability \eqn{Q}, computed by [F_available_habitat];
#' - the availability of ovitraps
#' - the availability of unsuitable habitats
#' - the availability of anything that attracts egg laying mosquitoes, including ovitraps and unsuitable habitats
#' - the egg distribution matrix \eqn{O}, made by [make_U_matrix]
#' - a vector that stores eggs laid
#'
#' This function is called by `compute_xds_object_template` to set up `egg_laying` and the variables and parameters with all
#' the variables it might depend on.
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** object
#' @importFrom Rdpack reprompt
#' @seealso The habitat membership matrix is created by [make_habitat_matrix()]
#' @keywords internal
#' @export
setup_egg_laying = function(xds_obj){
  
  # Eggs Laid x Patch
  xds_obj$terms$G <- list()
  xds_obj$terms$G[[1]] <- rep(0, xds_obj$nPatches)
  
  # Eggs Laid x Habitat
  xds_obj$terms$eta <- list()
  xds_obj$terms$eta[[1]] <- rep(0, xds_obj$nHabitats)
  
  # Emergence x Habitat
  xds_obj$terms$alpha <- list()
  xds_obj$terms$alpha[[1]] <- rep(0, xds_obj$nHabitats)
  
  # Emergence x Patch
  xds_obj$terms$Lambda <- list()
  xds_obj$terms$Lambda[[1]] <- rep(0, xds_obj$nPatches)
  
  # Habitat Availability
  xds_obj$terms$Q <- list()
  xds_obj$terms$Q[[1]] <- rep(1, xds_obj$nPatches)
  
  xds_obj$terms$Qall <- list()
  xds_obj$terms$Qall[[1]] <- rep(1, xds_obj$nPatches)
  
  # Bad Habitats
  xds_obj$patches$bad_habitats_obj = list()
  xds_obj$patches$bad_habitats_obj[[1]] = make_static_obj()
  
  xds_obj$patches$bad_habitats = list()
  xds_obj$patches$bad_habitats[[1]] = rep(0, xds_obj$nPatches)
  
  # Ovitraps
  
  xds_obj$patches$ovitraps_obj = list()
  xds_obj$patches$ovitraps_obj[[1]] = make_static_obj()
  
  xds_obj$patches$ovitraps = list()
  xds_obj$patches$ovitraps[[1]] = rep(0, xds_obj$nPatches)
  
  return(xds_obj)
}

#' @title The ML interface
#' @description
#' Egg laying and emergence 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @export
#' @keywords internal
ML_interface = function(t, y, xds_obj){
  xds_obj <- Emergence(t, y, xds_obj)
  xds_obj <- EggLaying(t, y, xds_obj)
  return(xds_obj)
}