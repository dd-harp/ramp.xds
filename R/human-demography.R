
#' @title Human (or Host) population birth rate
#'
#' @description This method dispatches on the type of xds_obj$Hpar$Births
#'
#' @param t current simulation time
#' @param H population density
#' @param births the birth function object
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births <- function(t, H, births){
  UseMethod("Births", births)
}

#' @title Human (or Host) population birth rate
#'
#' @description a function
#'
#' @inheritParams Births
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births.zero <- function(t, H, births="zero"){return(0*t)}


