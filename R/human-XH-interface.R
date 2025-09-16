# generic methods for human component

#' @title The **XH** Module Skill Set 
#' 
#' @description The **XH** skill set is a list of 
#' an module's capabilities: 
#' 
#' + `demography` is 
#'
#' @param Xname the **XH** module name 
#' 
#' @return *XH* module skill set, as a list 
#' 
#' @export
skill_set_XH = function(Xname){
  class(Xname) <- Xname
  UseMethod("skill_set_XH", Xname)
}

#' Check / update before solving 
#'
#' @param xds_obj an **`xds`** model object 
#' @param i host species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_XH = function(xds_obj,i){
  UseMethod("check_XH", xds_obj$XH_obj[[i]]) 
}

#' @title Compute **XH** Component Derivatives
#' 
#' @description Using the stored values 
#' of the daily FoI, compute the derivatives and 
#' return the derivatives as a numeric vector. 
#' 
#' @note This method dispatches on `class(xds_obj$XH_obj)` 
#'
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' 
#' @return the derivatives as a [numeric] vector 
#' 
#' @export
dXHdt <- function(t, y, xds_obj, i) {
  UseMethod("dXHdt", xds_obj$XH_obj[[i]])
}

#' @title Update X states for a discrete time system
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector
#' @export
Update_XHt <- function(t, y, xds_obj, i) {
  UseMethod("Update_XHt", xds_obj$XH_obj[[i]])
}



#' @title Size of effective infectious human population
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X <- function(t, y, xds_obj, i) {
  UseMethod("F_X", xds_obj$XH_obj[[i]])
}

#' @title Size of human population denominators
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H <- function(t, y, xds_obj, i) {
  UseMethod("F_H", xds_obj$XH_obj[[i]])
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @export
F_infectivity <- function(y, xds_obj, i) {
  UseMethod("F_infectivity", xds_obj$XH_obj[[i]])
}

#' @title Setup an **XH** Module (Human / Host Epidemiology & Demography)
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
setup_XH_obj = function(Xname, xds_obj, i, options=list()){
  class(Xname) <- Xname
  UseMethod("setup_XH_obj", Xname)
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
get_XH_vars <- function(y, xds_obj, i=1) {
  UseMethod("get_XH_vars", xds_obj$XH_obj[[i]])
}

#' @title Change human population density
#' @param H human population density
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
change_H = function(H, xds_obj, i=1){
  stopifnot(length(H) == xds_obj$nStrata[i])
  vars <- get_XH_inits(xds_obj,i)
  vars$H <- H 
  xds_obj <- change_XH_inits(xds_obj, i, vars)
  xds_obj$XY_interface <- trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}


#' @title Add indices for human population to parameter list
#' 
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`
#' 
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return an **`xds`** object
#' @export
setup_XH_ix <- function(xds_obj, i) {
  UseMethod("setup_XH_ix", xds_obj$XH_obj[[i]])
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
get_XH_ix <- function(xds_obj, i=1) {
  xds_obj$XH_obj[[i]]$ix
}


#' @title Parse **XH** Outputs 
#' 
#' @description After solving, this function extracts
#' the values of the dependent variables for the **XH** 
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
parse_XH_orbits <- function(outputs, xds_obj, i) {
  UseMethod("parse_XH_orbits", xds_obj$XH_obj[[i]])
}


#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
get_XH_pars <- function(xds_obj, i=1) {
  UseMethod("get_XH_pars", xds_obj$XH_obj[[i]])
}

#' @title Set new X parameter values
#' @description This method dispatches on the type of `xds_obj$XH_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param i the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_XH_pars <- function(xds_obj, i=1, options=list()) {
  UseMethod("change_XH_pars", xds_obj$XH_obj[[i]])
}


#' @title Setup Initial Values for **XH** Modules 
#' 
#' @description This method dispatches on `xds_obj$XH_obj[[i]]`.
#' 
#' @param xds_obj an **`xds`** object
#' @param H initial host population density
#' @param i the host species index
#' @param options a [list]
#' @return an **`xds`** object
#' @export
setup_XH_inits = function(xds_obj, H, i, options=list()){
  UseMethod("setup_XH_inits", xds_obj$XH_obj[[i]])
}


#' @title A function to set up XH_obj
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
get_XH_inits = function(xds_obj, i=1){
  xds_obj$XH_obj[[i]]$inits 
}


#' @title Set new X parameter values
#' @description This method dispatches on the type of `xds_obj$XH_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param i the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_XH_inits <- function(xds_obj, i=1, options=list()) {
  UseMethod("change_XH_inits", xds_obj$XH_obj[[i]])
}


#' @title Compute Net Infectiousness (NI)
#' @description A function to compute NI as an output
#' @param vars a list with the variables attached by name
#' @param XH_obj a list defining a model for human
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ni <- function(vars, XH_obj) {
  UseMethod("F_ni", XH_obj)
}

#' @title Compute the *true* prevalence of infection / parasite rate
#' @description A function that translates the state variables into
#' the "true" *Pf*PR
#' @param vars a list with the variables attached by name
#' @param XH_obj a list defining a model for human
#' @return a [numeric] vector of length `nStrata`
#' @export
F_prevalence <- function(vars, XH_obj) {
  UseMethod("F_prevalence", XH_obj)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_lm <- function(vars, XH_obj) {
  UseMethod("F_prevalence", XH_obj)
}

#' @title Compute the prevalence of infection by RDT
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by rapid diagnostic test (RDT)
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_rdt <- function(vars, XH_obj) {
  UseMethod("F_prevalence", XH_obj)
}

#' @title Compute infection prevalence by PCR
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by PCR
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_pcr <- function(vars, XH_obj) {
  UseMethod("F_prevalence", XH_obj)
}

#' Basic plotting for epidemiological models
#'
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add plot axes only if FALSE
#'
#' @export
xds_plot_X = function(xds_obj, i=1, clrs="black", llty=1, add=FALSE){
  UseMethod("xds_plot_X", xds_obj$XH_obj[[i]])
}

#' @title Compute the human transmitting capacity
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return none
#' @export
HTC <- function(xds_obj, i) {
  UseMethod("HTC", xds_obj$XH_obj[[i]])
}

#' @title Steady States for **X**
#' 
#' @description Compute the steady states as a function of the daily FoI for a
#' static value of human population density
#' 
#' @param foi the daily FoI
#' @param H human / host population density
#' @param XH_obj an **XH** model object
#' 
#' @return steady states 
#' @export
steady_state_X = function(foi, H, XH_obj){
  UseMethod("steady_state_X", XH_obj)
}

#' @title Steady States for **XH**
#' 
#' @description Compute the steady states as a function of the daily FoI for a
#' static value of human population density
#' 
#' @param foi the daily FoI
#' @param XH_obj an **XH** model object
#' 
#' @return steady states 
#' @export
steady_state_XH = function(foi, XH_obj){
  UseMethod("steady_state_XH", XH_obj)
}
