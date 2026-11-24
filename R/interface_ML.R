# X

#' @title Setup the Habitat Interface for Egg Laying and Emergence
#'
#' @description Set up a part of the `xds` object that defines the interface for egg laying
#' @details
#' This implements a model for egg laying described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in **`ramp.xds`** requires a rigid interface
#' to guarantee mathematical consistency for egg laying and emergence.
#' This function initializes lists in `xds_obj$terms` for the first vector species:
#' - `G`: eggs laid in each patch
#' - `eta`: eggs laid in each habitat
#' - `alpha`: emergence from each habitat
#' - `Lambda`: emergence in each patch
#' - `Q` and `Qall`: habitat availability in each patch, initialized to one
#'
#' The egg-laying and emergence terms are initialized to zero.
#' In `xds_obj$patches`, it initializes `bad_habitats` and `ovitraps`
#' to zero in each patch, with static port objects.
#'
#' This function is called by [make_xds_object_template].
#' Habitat membership is configured separately by [setup_habitats].
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds} }
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' @importFrom Rdpack reprompt
#' @seealso The habitat membership matrix is created by [make_habitat_matrix()]
#' @keywords internal
#' @export
setup_egg_laying_interface = function(xds_obj){
  
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
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @export
#' @keywords internal
ML_interface = function(t, y, xds_obj){
  xds_obj <- Emergence(t, y, xds_obj)
  xds_obj <- EggLaying(t, y, xds_obj)
  return(xds_obj)
}