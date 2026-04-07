
#' @title Update Blood Search Weights
#' @description Port function for blood feeding search weights, \eqn{\omega}.
#' Dispatches on `class(xds_obj$XH_obj[[i]]$search_obj)`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param i the host species index
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights <- function(xds_obj, s, i) {
  UseMethod("update_blood_search_weights", xds_obj$XH_obj[[i]]$search_obj)
}

#' @title Update Blood Search Weights (static)
#' @description Returns `xds_obj` unmodified; search weights are static.
#' @inheritParams update_blood_search_weights
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights.static <- function(xds_obj, s, i) { return(xds_obj) }

#' @title Change Blood Search Weights
#'
#' @description
#' Change the blood feeding search weights, \eqn{\omega},
#' for a set of host strata
#'
#' @param wts the blood feeding search weights
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_blood_search_weights = function(wts, xds_obj, s=1, i=1){
  wts = checkIt(wts, xds_obj$nStrata[i], fixit=TRUE) 
  xds_obj$XH_obj[[i]]$search_weights[[s]] = wts
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}
