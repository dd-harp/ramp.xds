
#' F_one
#'
#' @param t time
#'
#' @returns 1
#' @keywords internal
#' @export
F_one = function(t, V=list()){0*t+1}

#' F_zero
#'
#' @param t time
#'
#' @returns 0
#' @keywords internal
#' @export
F_zero = function(t, V=list()){0*t}


#' Get Variables
#' 
#' @description
#' A generic function to retrieve variables from the **`xds`** model object
#'
#' @param opts an options list and dispatching
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @returns a list of named variables
#' @keywords internal
#' @export
get_variables = function(opts, t, y, xds_obj, ix){
  UseMethod("get_variables", opts) 
}

#' Get Variables
#' 
#' @description
#' The NULL case for [get_variables]
#' 
#'
#' @inheritParams get_variables
#' 
#' @returns an empty list 
#' @keywords internal
#' @export
get_variables.list = function(opts, t, y, xds_obj, ix){
  list() 
}
