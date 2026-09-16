#' @title Malaria Importation 
#' 
#' @description
#' Importation is a junction. The function is called in [xds_compute_terms].
#' 
#'  
#' @name xds_info_importation
NULL


#' @title Setup importation
#'
#' @description
#' This function, called by [make_xds_object_template], sets up
#' terms and objects associated with malaria importation
#'
#' @details
#' This implements a framework to model blood feeding
#' described by Wu SL, *et al.*, (2023).
#'
#' Modular computation in **`ramp.xds`** requires a rigid interface to
#' guarantee mathematical consistency in computing quantites related to blood feeding and transmission.
#'
#' Several terms were developed to model importation by traveling humans and visitors. The terms
#' associated with time away and the travel EIR are human activities, so they are set up on the **XH** object.
#' This sets up visitors and infectiousness. 
#'
#' **Mulit-Host Models**
#'
#' In models with multiple host species, the availability of visitors might vary by host.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#'
#' @seealso [setup_transmission]
#'
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @keywords internal
#'
#' @export
#' @keywords internal
setup_importation <- function(xds_obj){
  with(xds_obj,{

    xds_obj$import_obj <- make_static_obj()
    
    xds_obj$patches$visitors_obj = list()
    xds_obj$patches$visitors_obj[[1]] = make_static_obj()
    
    xds_obj$patches$visitors = list()
    xds_obj$patches$visitors[[1]] =  rep(0, nPatches) 
    
    xds_obj$patches$vis_kappa_obj = list()
    xds_obj$patches$vis_kappa_obj[[1]] = make_static_obj()
    
    xds_obj$patches$vis_kappa = list()
    xds_obj$patches$vis_kappa[[1]] =  rep(0, nPatches) 

    return(xds_obj)
  })}



#' @title Importation Junction
#'
#' @description Dispatches on `class(xds_obj$import_obj)` to update
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
  UseMethod("Importation", xds_obj$import_obj)
}

#' @title Importation
#'
#' @description Importation for static models
#'
#' @inheritParams Importation
#' @return an **`xds`** object
#' @keywords internal
#' @export
Importation.static = function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Importation
#'
#' @description No-op method for the `Importation` junction when no
#' dynamic importation has been configured. The static default
#' parameters set by [setup_importation] are used as-is.
#'
#' @inheritParams Importation
#' @return an **`xds`** object
#' @keywords internal
#' @export
Importation.dynamic = function(t, y, xds_obj){
  importation_dynamics(t, y, xds_obj)
}

#' @title Importation terms
#' @description Compute terms associated with importation
#' 
#' @inheritParams Importation
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
importation_dynamics = function(t, y, xds_obj){
  for(i in 1:xds_obj$nHostSpecies){
    xds_obj <- update_visitors(t, y, xds_obj, i)
    xds_obj <- update_vis_kappa(t, y, xds_obj, i)
    xds_obj <- update_travel_eir(t, y, xds_obj, i)
    xds_obj <- update_time_away(t, y, xds_obj, i)
  }
  return(xds_obj)
}