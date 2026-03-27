#' @title Malaria Importation 
#' 
#' @description
#' Importation is a junction. The function is called in [xds_compute_terms].
#' 
#'  
#' @name xds_info_importation
NULL


#' @title Setup the Importation Object
#'
#' @description Setup an object to handle malaria importation
#' through travel and visitors. Initializes the junction and
#' zero-initializes the importation parameters on the `XY_interface`.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_importation_object = function(xds_obj){
  importation <- list()
  class(importation) <- 'none'
  importation$name <- "Junction: Importation"
  xds_obj$importation_obj <- importation

  xds_obj$XY_interface$time_at_home = list()
  xds_obj$XY_interface$time_at_home[[1]] = rep(1, xds_obj$nStrata)

  xds_obj$terms$travel_EIR <- list()
  xds_obj$terms$travel_EIR[[1]] <- rep(0, xds_obj$nStrata)

  xds_obj$XY_interface$visitors = list()
  xds_obj$XY_interface$visitors[[1]] = rep(0, xds_obj$nPatches)

  xds_obj$XY_interface$vis_kappa = list()
  xds_obj$XY_interface$vis_kappa[[1]] = rep(0, xds_obj$nPatches)

  return(xds_obj)
}

#' @title Importation Junction
#'
#' @description Dispatches on `class(xds_obj$importation_obj)` to update
#' importation-related parameters: time at home, travel EIR,
#' visitor availability, and visitor infectiousness.
#'
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
Importation = function(t, y, xds_obj){
  UseMethod("Importation", xds_obj$importation_obj)
}

#' @title Importation (none)
#'
#' @description No-op method for the `Importation` junction when no
#' dynamic importation has been configured. The static default
#' parameters set by [setup_importation_object] are used as-is.
#'
#' @inheritParams Importation
#' @return an **`xds`** object
#' @keywords internal
#' @export
Importation.none = function(t, y, xds_obj){
  return(xds_obj)
}
