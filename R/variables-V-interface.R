# Y 

#' @title Compute Other State Variables
#' @description
#' The description for each method should include the equations.
#'
#' @note This is the `S3` generic. Methods dispatch on `V_obj`.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the i\eqn{^{th}} set of auxiliary variables
#' @return derivatives for the auxiliary variables as a [vector]
#' @keywords internal
#' @export
dVdt <- function(t, y, xds_obj, i) {
  UseMethod("dVdt", xds_obj$V_obj[[i]])
}


#' @title Set up a **V** module
#' @description Set the parameter values and configure a model
#' for the **V** component.
#'
#' @param XHname the model name
#' @param xds_obj an **`xds`** model object
#' @param options model options as a named list
#' @param i the auxiliary-variable module index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_V_obj = function(XHname, xds_obj, options=list(), i=1){
  class(XHname) <- XHname
  UseMethod("setup_V_obj", XHname)
}

#' @title Get Variables by Name
#'
#' @description Get the values
#' of variables from the flat state
#' variable vector \eqn{y}, and return
#' the values as a named list.
#'
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the auxiliary-variable module index
#'
#' @return Variables as a named list
#'
#' @export
get_V_vars <- function(y, xds_obj, i=1) {
  UseMethod("get_V_vars", xds_obj$V_obj[[i]])
}

#' @title Add indices for auxiliary variables to the parameter list
#'
#' @description This method dispatches on the type of `xds_obj$V_obj[[i]]`.
#'
#' @param xds_obj an **`xds`** model object
#' @param i the auxiliary-variable module index
#' @return an **`xds`** model object
#' @export
setup_V_ix <- function(xds_obj, i) {
  UseMethod("setup_V_ix", xds_obj$V_obj[[i]])
}

#' @title Get indices for auxiliary variables
#'
#' @description Retrieve the values of the indices
#' for the variables.
#'
#' @param xds_obj an **`xds`** model object
#' @param i the auxiliary-variable module index
#'
#' @return the index [vector]
#' @export
get_V_ix <- function(xds_obj, i=1) {
  xds_obj$V_obj[[i]]$ix
}


#' @title Parse **V** Outputs
#'
#' @description After solving, this function extracts
#' the values of the dependent variables for the **V**
#' module from the matrix of solutions,
#' often called the "orbits."
#' The orbits are parsed
#' and attached by name to the **`xds`** model object.
#'
#' For differential equations
#' the solution matrix is returned by [deSolve].
#' Discrete-time systems return a matrix
#' that has the same shape.
#'
#' @param outputs an output matrix returned by [deSolve]
#' @param xds_obj an **`xds`** model object
#' @param i the auxiliary-variable module index
#'
#' @keywords internal
#' @export
parse_V_orbits <- function(outputs, xds_obj, i) {
  UseMethod("parse_V_orbits", xds_obj$V_obj[[i]])
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$V_obj[[i]]`.
#' @param xds_obj an **`xds`** model object
#' @param i the auxiliary-variable module index
#' @return a [list]
#' @export
get_V_pars <- function(xds_obj, i=1) {
  UseMethod("get_V_pars", xds_obj$V_obj[[i]])
}

#' @title Set new V parameter values
#' @description This method dispatches on the type of `xds_obj$V_obj[[i]]`.
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param i the auxiliary-variable module index
#' @return an **`xds`** model object
#' @export
change_V_pars <- function(xds_obj, options=list(), i=1) {
  UseMethod("change_V_pars", xds_obj$V_obj[[i]])
}


#' @title Set up initial values for **V** modules
#'
#' @description This method dispatches on `xds_obj$V_obj[[i]]`.
#'
#' @param xds_obj an **`xds`** model object
#' @param H initial host population density
#' @param options a [list]
#' @param i the auxiliary-variable module index
#' @return an **`xds`** model object
#' @export
setup_V_inits = function(xds_obj, H, options=list(), i=1){
  UseMethod("setup_V_inits", xds_obj$V_obj[[i]])
}


#' @title Get initial values for auxiliary variables
#'
#' @description Retrieve the stored
#' set of initial values.
#'
#' @param xds_obj an **`xds`** model object
#' @param i the auxiliary-variable module index
#'
#' @return the initial values, as a named list
#'
#' @export
get_V_inits = function(xds_obj, i=1){
  xds_obj$V_obj[[i]]$inits
}


#' @title Set new V initial values
#' @description This method dispatches on the type of `xds_obj$V_obj[[i]]`.
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param i the auxiliary-variable module index
#' @return an **`xds`** model object
#' @export
change_V_inits <- function(xds_obj, options=list(), i=1) {
  UseMethod("change_V_inits", xds_obj$V_obj[[i]])
}
