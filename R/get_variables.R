
#' Get Variables
#' 
#' @description
#' A generic function to retrieve variables from the **`xds`** model object
#'
#' @param opts an options list and dispatching
#' @param t time
#' @param y dependent variables
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