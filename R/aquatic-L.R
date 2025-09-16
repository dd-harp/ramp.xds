# generic methods for aquatic component

#' @title The **L** Module Skill Set 
#' 
#' @description The **L** skill set is a list of 
#' an module's capabilities: 
#' 
#' + `demography` is 
#'
#' @param Lname the **L** module name 
#' 
#' @return *L* module skill set, as a list 
#' 
#' @export
skill_set_L = function(Lname){
  class(Lname) <- Lname
  UseMethod("skill_set_L", Lname)
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_L = function(xds_obj, s){
  UseMethod("check_L", xds_obj$L_obj[[s]]) 
}

#' @title Derivatives for an **L** Component Module
#' @description This method computes and returns the derivatives
#' for the **L** Component modules
#' @note Dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @return the derivatives, a [numeric] vector of length \eqn{n_q=}`nHabitats`
#' @export
dLdt <- function(t, y, xds_obj, s) {
  UseMethod("dLdt", xds_obj$L_obj[[s]])
}

#' @title Update State Variables for an **L** Component Module
#' @description This method updates the state variables
#' for **L** Component modules
#' @note Dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @return the states, a [numeric] vector of length \eqn{n_q=}`nHabitats`
#' @export
Update_Lt <- function(t, y, xds_obj, s) {
  UseMethod("Update_Lt", xds_obj$L_obj[[s]])
}


#' @title Baseline Bionomics for an **L** Component Module
#' @description Handle immature mosquito bionomic parameters as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
LBaseline <- function(t, y, xds_obj, s) {
  UseMethod("LBaseline", xds_obj$L_obj[[s]])
}


#' @title Bionomics for an **L** Component Module
#' @description Handle immature mosquito bionomic parameters as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
LBionomics <- function(t, y, xds_obj, s) {
  UseMethod("LBionomics", xds_obj$L_obj[[s]])
}

#' @title Compute Emergent Adults
#' @description This function computes the rate or number of emerging adults: a
#' rate for differential equations, or a number for difference equations.
#' @note This method dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge <- function(t, y, xds_obj, s) {
  UseMethod("F_emerge", xds_obj$L_obj[[s]])
}

#' @title Set up `L_obj` for **L** Component modules
#' @description
#' Each instance of `setup_L_obj.*` calls a function `create_L_obj_*` that
#' creates an object **`L_obj`.**
#' It is attached the **`xds`** object as `xds_obj$L_obj[[s]].` Each instance of `create_L_obj_*`
#' should assign default parameter values that will be over-written by `options`
#' @note This method assigns `Lname` to class(`Lname`) and dispatches on `Lname`.
#' @param Lname the class name of the aquatic model
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @param options a named [list] to configure **`L_obj`**
#' @return an **`xds`** object
#' @export
setup_L_obj = function(Lname, xds_obj, s, options=list()){
  class(Lname) <- Lname
  UseMethod("setup_L_obj", Lname)
}

#' @title Get parameters for the **L** Component module
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_L_pars <- function(xds_obj, s=1) {
  UseMethod("get_L_pars", xds_obj$L_obj[[s]])
}

#' @title Set **L** Component Parameters
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_L_pars <- function(xds_obj, s=1, options=list()) {
  UseMethod("change_L_pars", xds_obj$L_obj[[s]])
}

#' @title List **L** Component Variables
#' @description Extract the variables describing **L** Component
#' states for the \eqn{s^{th}} species and return them as a named list
#' @note This method dispatches on the class of `xds_obj$L_obj[[s]]`.
#' @param y the variables
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return a named [list]: the variables of \eqn{\cal L} by name
#' @export
get_L_vars <- function(y, xds_obj, s) {
  UseMethod("get_L_vars", xds_obj$L_obj[[s]])
}

#' @title Setup Initial Values for the **L** Component
#' @description This method dispatches on `Lname`.
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @param options a [list]
#' @return an **`xds`** object
#' @export
setup_L_inits = function(xds_obj, s, options=list()){
  UseMethod("setup_L_inits", xds_obj$L_obj[[s]])
}

#' @title Get Initial Values for the **L** Component
#' @note This method dispatches on the class of `xds_obj$L_obj`.
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @return a named [list]
#' @export
get_L_inits <- function(xds_obj, s=1) {
  xds_obj$L_obj[[s]]$inits
}

#' @title Set **L** Component Initial Values
#' @description
#' This sets initial values for an **L** Component
#' module from a named list
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_L_inits <- function(xds_obj, s=1, options=list()) {
  UseMethod("change_L_inits", xds_obj$L_obj[[s]])
}

#' @title Set the Values of the Indices for **L** Component Modules
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @return an **`xds`** object
#' @export
setup_L_ix <- function(xds_obj, s) {
  UseMethod("setup_L_ix", xds_obj$L_obj[[s]])
}

#' @title parse **L** Component Outputs
#' @description After solving a dynamical system, parse the outputs and
#' return the variables by name in a list.
#' @note This method dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param outputs a [matrix] with the solutions to a dynamical system
#' @param xds_obj an **`xds`** object
#' @param s the species index
#' @return a [list]
#' @export
parse_L_orbits <- function(outputs, xds_obj, s) {
  UseMethod("parse_L_orbits", xds_obj$L_obj[[s]])
}

#' @title Compute steady states for **L** Component Modules
#' @description For differential equation models, compute
#' steady states as a function of daily eggs laid, \eqn{\eta}
#' @note This method dispatches on the class of `L_obj`.
#' @param eta the egg-laying rate
#' @param L_obj a list that defines an xde model
#' @return a named [list]: values of the state variables at the steady state
#' @export
steady_state_L = function(eta, L_obj){
  UseMethod("steady_state_L", L_obj)
}
