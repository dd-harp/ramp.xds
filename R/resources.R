#' @title Setup Resources Object
#'
#' @description Resources is a junction with
#' ports for several objects that are part of the
#' blood feeding or habitat interface.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_resources_object = function(xds_obj){
  xds_obj <- setup_sugar_object(xds_obj)
  xds_obj <- setup_traps_object(xds_obj)
  return(xds_obj)
}

#' @title Resources
#'
#' @description Computes availability of resources potentially
#' affecting adult mosquito bionomics: blood hosts,
#' aquatic habitats, and sugar. There are ports to
#' compute variables that are part of the blood feeding,
#' egg laying, or exposure interfaces.
#'
#' + [BloodHosts] availability of alternative blood hosts
#' + [HabitatDynamics] to modify habitat search weights
#' + [Traps] to compute availability of oviposition traps
#' + [Sugar] availability of sugar
#'
#' @note [Travel] and [Visitors] are called separately in
#' [xds_compute_terms], not inside [Resources].
#'
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
Resources = function(t, y, xds_obj){
  xds_obj <- BloodHosts(t, y, xds_obj)
  xds_obj <- HabitatDynamics(t, y, xds_obj)
  xds_obj <- Traps(t, y, xds_obj)
  xds_obj <- Sugar(t, y, xds_obj)
  return(xds_obj)
}
