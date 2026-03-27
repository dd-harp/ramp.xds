#' @title Setup Resources Object
#'
#' @description Resources is a junction. When set to its default class (`none`),
#' all resource parameters (blood hosts, habitat weights, traps, sugar) retain
#' their static zero-initialized defaults. Dynamic port machinery is provided
#' by satellite packages (e.g., `ramp.forcing`).
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_resources_object = function(xds_obj){
  resources <- list()
  class(resources) <- 'none'
  resources$name <- "Junction: Resources"
  xds_obj$resources_obj <- resources
  return(xds_obj)
}

#' @title Resources
#'
#' @description Resources is a junction that optionally updates parameters
#' affecting adult mosquito bionomics: blood hosts,
#' aquatic habitat dynamics, traps, and sugar. 
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
  UseMethod("Resources", xds_obj$resources_obj)
}

#' @title Resources (none)
#'
#' @description The default no-op for Resources. All resource parameters
#' retain their static defaults (zero-initialized by [setup_XY_interface]
#' and [setup_ML_interface]).
#'
#' @inheritParams Resources
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
Resources.none = function(t, y, xds_obj){
  return(xds_obj)
}
