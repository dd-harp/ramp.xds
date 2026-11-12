# generic methods for aquatic component

#' @title Setup the skill set
#'
#' @description The skill set for an **L** module
#' is a list that summarizes capabilities and
#' compatibilities
#'
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#'
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
setup_skillset_L = function(xds_obj, s){
  UseMethod("setup_skillset_L", xds_obj$L_obj[[s]])
}

#' @title Run checks before solving (**L**)
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


#' @title Mosquito bionomics (**L**)
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

#' @title Set up an **L** model object 
#' 
#' @description
#' This setup creates an **L** object for the \eqn{s^{th}} species
#' and sets up the interface ports: 
#' + calls `make_L_obj_`*Lname* to set up the dynamics, including parameters
#' + calls [setup_L_inits] to set initial values
#' + calls [setup_habitat_search_weights]  
#' 
#' The function is typically called by [xds_setup], but it is also used to add vector species. 
#' 
#' @seealso [xds_setup], [change_L_pars], [setup_L_inits], [setup_habitat_search_weights]
#' 
#' @note The model object is stored as `xds_obj$L_obj[[s]]`
#'
#' @param Lname the class name of the **L** module
#' @param membership the habitats' patch membership vector
#' @param xds_obj an **`xds`** model object
#' @param options a named list to configure **`L_obj`**
#' @param s the species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_obj = function(Lname, membership, xds_obj, options=list(), s=1){
  class(Lname) <- Lname
  UseMethod("setup_L_obj", Lname)
}

#' @title Get parameters (**L**)
#' 
#' @description Inspect the parameter values for the **L** module for the \eqn{s^{th}} species
#' 
#' @seealso [change_L_pars], [setup_L_obj], [xds_info_change_parameters]
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
get_L_pars <- function(xds_obj, s=1) {
  UseMethod("get_L_pars", xds_obj$L_obj[[s]])
}

#' @title Change parameters (**L**)
#' 
#' @description
#' For the **L** module for the \eqn{s^{th}} species, 
#' change parameter values named in `options`,
#' leaving other values unchanged.
#' 
#' @seealso [get_L_pars], [setup_L_obj], [xds_info_change_parameters]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_pars <- function(xds_obj, options=list(), s=1) {
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

#' @title Setup initial values (**L**)
#' 
#' @description Set initial values
#' for all the variables.  
#' + Each module (*Lname*) defines
#' a function `make_L_inits_`*Lname* 
#' that supplies default values
#' + The values in `options` overwrite the defaults
#' + To inspect variable names, use [get_L_inits] or 
#' see the inline module documentation.
#' 
#' @seealso [get_L_inits], [xds_info_initial_conditions]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named [list]
#' @param s the species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_L_inits = function(xds_obj, options=list(), s=1){
  UseMethod("setup_L_inits", xds_obj$L_obj[[s]])
}

#' @title Get initial values (**L**) 
#' 
#' @description Inspect the initial values for the **L** module
#' 
#' @seealso [change_L_inits], [setup_L_inits]
#' @note The initial values are stored as a named list called `xds_obj$L_obj[[s]]$inits`
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a named [list]
#' @keywords internal
#' @export
get_L_inits <- function(xds_obj, s=1) {
  xds_obj$L_obj[[s]]$inits
}

#' @title Change initial values (**L**)
#' 
#' @description
#' Change the stored initial values for 
#' any variables named in `options`,
#' leaving other values unchanged.
#' 
#' @seealso [get_L_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_L_inits <- function(xds_obj, options=list(), s=1) {
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

#' @title Get indices (**L**)
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


#' @title Get orbits (**L**)
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