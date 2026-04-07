
#' @title Set up L ports 
#'
#' @description
#' Adds port objects and default values for the habitat search weights.  
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_ports = function(xds_obj, s=1){
  nHabitats = xds_obj$nHabitats[s] 
  
  # habitat feeding search weights: one vector per vector species, dispatched by search_obj
  xds_obj$L_obj[[s]]$search_weights <- list()
  xds_obj$L_obj[[s]]$search_weights[[1]] <- rep(1, nHabitats)
  
  search_obj <- list()
  class(search_obj) <- "static"
  xds_obj$L_obj[[s]]$search_obj <- search_obj
  
  return(xds_obj)
}


#' @title Update habitat search weights
#' @description Port function for habitat feeding search weights, \eqn{\omega}.
#' Dispatches on `class(xds_obj$L_obj[[s]]$search_obj)`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_habitat_search_weights <- function(xds_obj, s) {
  UseMethod("update_habitat_search_weights", xds_obj$L_obj[[s]]$search_obj)
}

#' @title Update habitat search weights (static)
#' @description Returns `xds_obj` unmodified; search weights are static.
#' @inheritParams update_habitat_search_weights
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_habitat_search_weights.static <- function(xds_obj, s) { return(xds_obj) }

#' @title Change habitat search weights
#'
#' @description
#' Change the habitat feeding search weights, \eqn{\omega},
#' for a set of host strata
#'
#' @param wts the habitat feeding search weights
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @export
change_habitat_search_weights = function(wts, xds_obj, s=1){
  wts = checkIt(wts, xds_obj$nHabitats[s], fixit=TRUE) 
  xds_obj$L_obj[[s]]$search_weights = wts
  xds_obj$ML_interface = trigger_setup(xds_obj$ML_interface)
  return(xds_obj)
}
