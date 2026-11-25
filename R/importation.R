# X

#' @title Malaria Importation 
#' 
#' @description
#' 
#' Malaria can be imported from outside an area when residents acquire infections
#' while traveling or when infected visitors spend time in the area, get bitten by
#' mosquitoes, and infect them.
#'
#' For the travel pathway, see [travel_malaria]. Importation through infectious
#' visitors depends on [visitor_availability] and [visitor_infectiousness].
#'
#' The importation junction represents these processes using four terms:
#'
#' \describe{
#'   \item{[time_away]}{the fraction of time residents spend outside the area}
#'   \item{[travel_eir]}{the EIR residents experience while traveling}
#'   \item{[visitor_availability]}{the availability of non-resident humans to mosquitoes}
#'   \item{[visitor_infectiousness]}{the probability a mosquito becomes infected after feeding on a visitor}
#' }
#'
#' These parameters and variables are fully integrated into the interfaces that compute
#' blood feeding, transmission, and exposure. By default, they are set to have no effect,
#' but they can be
#' configured using setup functions.
#'
#' @note
#' [Importation] is called by [xds_compute_terms].
#'
#' There are no terms to model importation by immigrating infected mosquitoes.
#'
#' @seealso
#' + **Terms** --- [time_away], [travel_eir], [visitor_availability],
#'   [visitor_infectiousness]
#' + **Setup** --- [setup_visitors], [setup_vis_kappa], [setup_time_away],
#'   [setup_travel_eir]
#' + **Get and change** --- [get_visitors], [change_visitors],
#'   [get_vis_kappa], [change_vis_kappa], [get_time_away],
#'   [change_time_away], [get_travel_eir], [change_travel_eir]
#' + **Computation** --- [update_visitors], [update_vis_kappa], [update_time_away],
#'   [update_travel_eir], [importation_dynamics]
#'
#' @name malaria_importation
#' @aliases xds_info_importation xds_info_malaria_importation
NULL


#' @title Setup importation
#'
#' @description
#' This function, called by [make_xds_object_template], sets up
#' terms and objects associated with malaria importation.
#'
#' @details
#' Initialize the importation junction with a static object.
#' Visitor availability and visitor infectiousness are initialized to zero
#' in each patch for the first host species, with static port objects.
#' Time away and travel EIR are configured separately on the **XH** object.
#'
#' **Multi-Host Models**
#'
#' In models with multiple host species, the availability of visitors might vary by host.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#'
#' @seealso [setup_transmission]
#'
#' @references{\insertRef{WuSL2023SpatialDynamics}{ramp.xds}}
#' @keywords internal
#'
#' @export
#' @keywords internal
setup_importation_obj <- function(xds_obj){
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
#' @param t time
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
Importation = function(t, xds_obj){
  UseMethod("Importation", xds_obj$import_obj)
}

#' @title Importation
#'
#' @description Importation for static models
#'
#' @inheritParams Importation
#' @return an **`xds`** model object
#' @keywords internal
#' @export
Importation.static = function(t, xds_obj){
  return(xds_obj)
}

#' @title Importation
#'
#' @description Call [importation_dynamics] to update visitor availability,
#' visitor infectiousness, time away, and travel EIR for each host species.
#'
#' @inheritParams Importation
#' @return an **`xds`** model object
#' @keywords internal
#' @export
Importation.dynamic = function(t, xds_obj){
  importation_dynamics(t, xds_obj)
}


#' @title Importation terms
#' @description Compute terms associated with importation
#'
#' @section Importation:
#'
#' For each host species, this function calls four ports at time `t`:
#'
#' + [update_visitors] updates the availability of non-resident hosts.
#' + [update_vis_kappa] updates the net infectiousness of those visitors.
#' + [update_time_away] updates the fraction of time resident hosts spend away.
#' + [update_travel_eir] updates their EIR while traveling.
#'
#' Each port returns an updated `xds_obj`, which is passed to the next port.
#' Static ports leave the model unchanged.
#' These inputs are used by the blood-feeding and transmission interfaces
#' to account for visitors and by [Exposure] to account for infection risk
#' while traveling. This function updates the inputs; it does not itself
#' compute new infections or change the dependent variables.
#'
#' @inheritParams Importation
#' @seealso [malaria_importation], [time_away], [travel_eir],
#'   [visitor_availability], [visitor_infectiousness],
#'   [setup_visitors], [setup_vis_kappa], [setup_time_away],
#'   [setup_travel_eir], [get_visitors], [change_visitors],
#'   [get_vis_kappa], [change_vis_kappa], [get_time_away],
#'   [change_time_away], [get_travel_eir], [change_travel_eir]
#'
#' @return an **`xds`** model object
#' @export
#' @keywords internal
importation_dynamics = function(t, xds_obj){
  for(i in 1:xds_obj$nHostSpecies){
    xds_obj <- update_visitors(t, xds_obj, i)
    xds_obj <- update_vis_kappa(t, xds_obj, i)
    
    xds_obj <- update_time_away(t, xds_obj, i)
    xds_obj <- update_travel_eir(t, xds_obj, i)
  }
  return(xds_obj)
}
