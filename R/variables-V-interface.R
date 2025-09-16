
#' @title Compute Other Variables 
#' @description
#' The description for each method should include the equations.
#'
#' @note This is the `S3` generic. Methods dispatch on `V_obj`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an `xds` object
#' @param i the i^th auxiliary variable 
#' @return derivatives for the \eqn{\cal MYZ} component as a [vector]
#' @export
dVdt <- function(t, y, xds_obj, i) {
  UseMethod("dVdt", xds_obj$V_obj[[i]])
}


#' @title Setup an **V** Module (Human / Host Epidemiology & Demography)
#' @description Set the parameter values and configure a model 
#' for the **X** Component
#' 
#' @param Xname the model name 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options model options as a named list
#' 
#' @return an **`xds`** object
#' 
#' @export
setup_V_obj = function(Xname, xds_obj, i, options=list()){
  class(Xname) <- Xname
  UseMethod("setup_V_obj", Xname)
}

#' @title Get Variables by Name 
#' 
#' @description Get the the values
#' of variables from the flat state 
#' variable vector \eqn{y}, and return 
#' the values as a named list 
#' 
#' @param y the variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return Variables as a named list
#'  
#' @export
get_V_vars <- function(y, xds_obj, i=1) {
  UseMethod("get_V_vars", xds_obj$V_obj[[i]])
}

#' @title Add indices for human population to parameter list
#' 
#' @description This method dispatches on the type of `xds_obj$V_obj[[i]]`
#' 
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return an **`xds`** object
#' @export
setup_V_ix <- function(xds_obj, i) {
  UseMethod("setup_V_ix", xds_obj$V_obj[[i]])
}

#' @title Add indices for human population to parameter list
#' 
#' @description Get and display the values of the indices
#' for the variables   
#' 
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' 
#' @return an **`xds`** object
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
#' Discrete time systems return a matrix 
#' that has the same shape. 
#' 
#' @param outputs an output matrix returned by [deSolve]
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @export
parse_V_orbits <- function(outputs, xds_obj, i) {
  UseMethod("parse_V_orbits", xds_obj$V_obj[[i]])
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$V_obj[[i]]`.
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
get_V_pars <- function(xds_obj, i=1) {
  UseMethod("get_V_pars", xds_obj$V_obj[[i]])
}

#' @title Set new X parameter values
#' @description This method dispatches on the type of `xds_obj$V_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param i the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_V_pars <- function(xds_obj, i=1, options=list()) {
  UseMethod("change_V_pars", xds_obj$V_obj[[i]])
}


#' @title Setup Initial Values for **V** Modules 
#' 
#' @description This method dispatches on `xds_obj$V_obj[[i]]`.
#' 
#' @param xds_obj an **`xds`** object
#' @param H initial host population density
#' @param i the host species index
#' @param options a [list]
#' @return an **`xds`** object
#' @export
setup_V_inits = function(xds_obj, H, i, options=list()){
  UseMethod("setup_V_inits", xds_obj$V_obj[[i]])
}


#' @title A function to set up V_obj
#' 
#' @description Get and display the stored 
#' set of initial values. 
#'  
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return the initial values, as a named list
#' 
#' @export
get_V_inits = function(xds_obj, i=1){
  xds_obj$V_obj[[i]]$inits 
}


#' @title Set new X parameter values
#' @description This method dispatches on the type of `xds_obj$V_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param i the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_V_inits <- function(xds_obj, i=1, options=list()) {
  UseMethod("change_V_inits", xds_obj$V_obj[[i]])
}
