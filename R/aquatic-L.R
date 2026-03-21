# generic methods for aquatic component

#' @title The skill set (**L** module)
#'
#' @description The skill set for an **L** module
#' is a list that summarizes capabilities and
#' compatibilities
#'
#' @param Lname the **L** module name
#'
#' @return *L* module skill set, as a list
#'
#' @keywords internal
#' @export
skill_set_L = function(Lname){
  class(Lname) <- Lname
  UseMethod("skill_set_L", Lname)
}

#' @title Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_L = function(xds_obj, s){
  UseMethod("check_L", xds_obj$L_obj[[s]])
}

#' @title Compute derivatives (**L**)
#'
#' @description This method computes and returns the derivatives
#' for the **L** Component modules
#'
#' @note Dispatches on the class of `xds_obj$L_obj[[s]]`
#'
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return the derivatives, a [numeric] vector of length \eqn{n_q=}`nHabitats`
#' @keywords internal
#' @export
dLdt <- function(t, y, xds_obj, s) {
  UseMethod("dLdt", xds_obj$L_obj[[s]])
}

#' @title Update state variables (**L**)
#'
#' @description This method updates the state variables
#' for **L** Component modules
#'
#' @note Dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return the states, a [numeric] vector of length \eqn{n_q=}`nHabitats`
#' @keywords internal
#' @export
Update_Lt <- function(t, y, xds_obj, s) {
  UseMethod("Update_Lt", xds_obj$L_obj[[s]])
}


#' @title Immature mosquito bionomics
#' @description Compute the values of bionomic parameter 
#' values for an **L** Component module
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
LBionomics <- function(t, y, xds_obj, s) {
  UseMethod("LBionomics", xds_obj$L_obj[[s]])
}


#' @title Apply effect sizes (**L**)
#' @description Apply vector control effect sizes to immature mosquito bionomic parameters
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
LEffectSizes <- function(t, y, xds_obj, s) {
  UseMethod("LEffectSizes", xds_obj$L_obj[[s]])
}

#' @title Compute emergent adults
#' @description This function computes the rate or number of emerging adults: a
#' rate for differential equations, or a number for difference equations.
#' @note This method dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
F_emerge <- function(t, y, xds_obj, s) {
  UseMethod("F_emerge", xds_obj$L_obj[[s]])
}

#' @title Set up `L_obj` for **L** component modules
#' @description
#' Each instance of `setup_L_obj.*` calls a function `create_L_obj_*` that
#' creates an object **`L_obj`.**
#' It is attached the **`xds`** object as `xds_obj$L_obj[[s]].` Each instance of `create_L_obj_*`
#' should assign default parameter values that will be over-written by `options`
#' @note This method assigns `Lname` to class(`Lname`) and dispatches on `Lname`.
#'
#' @param Lname the class name of the **L** module
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @param options a named list to configure **`L_obj`**
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_obj = function(Lname, xds_obj, s, options=list()){
  class(Lname) <- Lname
  UseMethod("setup_L_obj", Lname)
}

#' @title Get parameters for the **L** component module
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
get_L_pars <- function(xds_obj, s=1) {
  UseMethod("get_L_pars", xds_obj$L_obj[[s]])
}

#' @title Set **L** component parameters
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a named list
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_pars <- function(xds_obj, s=1, options=list()) {
  UseMethod("change_L_pars", xds_obj$L_obj[[s]])
}

#' @title List variables (**L**)
#' @description Extract the variables describing **L** Component
#' states for the \eqn{s^{th}} species and return them as a named list
#' @note This method dispatches on the class of `xds_obj$L_obj[[s]]`.
#' @param y the variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list]: the variables of \eqn{\cal L} by name
#' @keywords internal
#' @export
get_L_vars <- function(y, xds_obj, s) {
  UseMethod("get_L_vars", xds_obj$L_obj[[s]])
}

#' @title Setup initial values for the **L** component
#' @description Sets the initial values of the **L** Component
#' variables from `options`, a named list.
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @param options a named [list]
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_inits = function(xds_obj, s, options=list()){
  UseMethod("setup_L_inits", xds_obj$L_obj[[s]])
}

#' @title Get initial values for the **L** component
#' @description The initial values are stored on the **L** object as a
#' named list called `inits`; `get_L_inits` returns that list.
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a named [list]
#' @keywords internal
#' @export
get_L_inits <- function(xds_obj, s=1) {
  xds_obj$L_obj[[s]]$inits
}

#' @title Set **L** component initial values
#' @description
#' This sets initial values for an **L** Component
#' module from a named list
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a named list
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_inits <- function(xds_obj, s=1, options=list()) {
  UseMethod("change_L_inits", xds_obj$L_obj[[s]])
}

#' @title Set up indices for **L** component variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_ix <- function(xds_obj, s) {
  UseMethod("setup_L_ix", xds_obj$L_obj[[s]])
}

#' @title Get L indices
#'
#' @description The indices are stored on the **L** object as a
#' named list called `ix`; `get_L_ix` returns that list.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a named [list] of indices for the **L** variables
#' @keywords internal
#' @export
get_L_ix <- function(xds_obj, s=1) {
  xds_obj$L_obj[[s]]$ix
}

#' @title Parse outputs (**L**)
#' @description After solving a dynamical system, parse the outputs and
#' return the variables by name in a list.
#' @note This method dispatches on the class of `xds_obj$L_obj[[s]]`
#' @param outputs a [matrix] with the solutions to a dynamical system
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a [list]
#' @keywords internal
#' @export
parse_L_orbits <- function(outputs, xds_obj, s) {
  UseMethod("parse_L_orbits", xds_obj$L_obj[[s]])
}

#' @title Compute steady states for **L** component modules
#'
#' @description For differential equation models, compute
#' steady states as a function of daily eggs laid, \eqn{\eta}
#'
#' @note This method dispatches on the class of `L_obj`.
#'
#' @param eta the egg-laying rate
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#'
#' @return a named [list]: values of the state variables at the steady state
#' @keywords internal
#' @export
steady_state_L = function(eta, xds_obj, s=1){
  UseMethod("steady_state_L", xds_obj$L_obj[[s]])
}


#' @title Get **L** outputs
#'
#' @description
#' Pull the saved, parsed orbits for the **L**
#' component from an **`xds`** object
#'
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the orbits for the **L** component
#'
#' @keywords internal
#' @export
get_L_orbits = function(xds_obj, s=1){

  got = xds_obj$outputs$orbits$L[[s]]
  got$time = xds_obj$outputs$time

  return(got)
}