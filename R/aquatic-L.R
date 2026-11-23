# X
# TODO: Complete the deferred check_L family description review across modules.
# generic methods for aquatic component


#' @title Compute the derivatives (**L**)
#'
#' @description Compute the derivatives of the aquatic mosquito state variables
#' using the selected **L** module. See the module-specific documentation for
#' equations and references.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a [numeric] vector of derivatives in the module's state-variable
#' order; empty for modules without state variables
#' @keywords internal
#' @export
dLdt <- function(t, y, xds_obj, s) {
  UseMethod("dLdt", xds_obj$L_obj[[s]])
}

#' @title Update states (**L**)
#'
#' @description Compute the updated aquatic mosquito states for a discrete-time
#' step using the selected **L** module.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a [numeric] vector of updated states in the module's state-variable
#' order; empty for modules without state variables
#' @keywords internal
#' @export
Update_Lt <- function(t, y, xds_obj, s) {
  UseMethod("Update_Lt", xds_obj$L_obj[[s]])
}

#' @title Get the state variables (**L**)
#' @description Extract the state variables for the selected **L** module and vector species
#' from `y` and return them by name in a list for use during model solving.
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$L_obj[[s]]`.
#' These functions support solver evaluations that need variables by name;
#' users rarely need to call them directly.
get_L_vars <- function(y, xds_obj, s) {
  UseMethod("get_L_vars", xds_obj$L_obj[[s]])
}


#' @title Mosquito bionomics (**L**)
#' @description Compute baseline bionomic parameter values
#' for an **L** component module.
#' Store each port-backed parameter `x` as `x_t` and reset `es_x` to 1.
#' Parameters without ports are stored directly as `x`.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' @seealso [LEffectSizes], [MosquitoBionomics]
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
LBionomics <- function(t, y, xds_obj, s) {
  UseMethod("LBionomics", xds_obj$L_obj[[s]])
}


#' @title Apply effect sizes (**L**)
#' @description Apply vector control effect sizes to baseline aquatic mosquito bionomic parameter values.
#' For each port-backed parameter, set `x = x_t * es_x`.
#' Leave parameters without ports unchanged.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' @seealso [LBionomics], [VectorControlEffectSizes]
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
LEffectSizes <- function(t, y, xds_obj, s) {
  UseMethod("LEffectSizes", xds_obj$L_obj[[s]])
}

#' @title Compute adult mosquito emergence (**L**)
#' @description Return adult female mosquito emergence per habitat.
#' This is a rate for differential equations or a number for discrete-time systems.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' @seealso [Emergence]
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a [numeric] vector of length `nHabitats`
#' @keywords internal
#' @export
F_emerge <- function(t, y, xds_obj, s) {
  UseMethod("F_emerge", xds_obj$L_obj[[s]])
}

#' @title Set up the module object (**L**)
#' 
#' @description
#' Set up the selected module and its interface ports for the
#' \eqn{s^{th}} vector species in the **L** component.
#'
#' + Pass `membership`, `xds_obj`, `options`, and `s` to the selected setup method.
#' + Let that method construct the module object and configure its
#'   initial values and habitat interface as appropriate for the module.
#' + Return the updated **`xds`** model object.
#' 
#' The function is typically called by [xds_setup], but it is also used to add vector species. 
#' 
#' @seealso [xds_setup], [change_L_pars], [setup_L_inits], [setup_habitat_search_weights]
#' 
#' @note The module object is stored at `xds_obj$L_obj[[s]]`.
#' Dispatch is on `Lname`, whose class is set to the supplied module name,
#' not on an existing module object.
#'
#' @param Lname the class name of the **L** module
#' @param membership the habitats' patch membership vector
#' @param xds_obj an **`xds`** model object
#' @param options a named list to configure **`L_obj`**
#' @param s the species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_L_obj = function(Lname, membership, xds_obj, options=list(), s=1){
  class(Lname) <- Lname
  UseMethod("setup_L_obj", Lname)
}

#' @title Get parameters (**L**)
#' 
#' @description Return the parameters for the selected **L** module
#' for the \eqn{s^{th}} vector species as a named list.
#'
#' @note Parameter values are stored on `xds_obj$L_obj[[s]]`.
#' Dispatch is on the class of `xds_obj$L_obj[[s]]`.
#' 
#' @seealso [change_L_pars], [setup_L_obj], [xds_change_parameters]
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of parameter values
#' @keywords internal
#' @export
get_L_pars <- function(xds_obj, s=1) {
  UseMethod("get_L_pars", xds_obj$L_obj[[s]])
}

#' @title Change parameters (**L**)
#' 
#' @description Change parameter values named in `options` for the selected
#' **L** module for the \eqn{s^{th}} vector species, leaving other values unchanged.
#'
#' @note Dispatch is on the class of `xds_obj$L_obj[[s]]`.
#' 
#' @seealso [get_L_pars], [setup_L_obj], [xds_change_parameters]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
change_L_pars <- function(xds_obj, options=list(), s=1) {
  UseMethod("change_L_pars", xds_obj$L_obj[[s]])
}

#' @title Set up initial values (**L**)
#' 
#' @description Set up initial values for the selected **L** module and
#' vector species. Values in `options` override the defaults.
#' @seealso [get_L_inits], [xds_initial_conditions]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named [list]
#' @param s the species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$L_obj[[s]]`.
#' @seealso [change_L_inits]
setup_L_inits = function(xds_obj, options=list(), s=1){
  UseMethod("setup_L_inits", xds_obj$L_obj[[s]])
}

#' @title Get initial values (**L**)
#' 
#' @description Return the stored initial values for the selected **L** module
#' and vector species.
#' 
#' @seealso [change_L_inits], [setup_L_inits]
#' @note Initial values are stored at `xds_obj$L_obj[[s]]$inits`.
#' An empty list, as in the `trivialL` module, is returned unchanged.
#' If the field is absent, return `NULL`.
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of initial values, or `NULL` if the field is absent
#' @keywords internal
#' @export
get_L_inits <- function(xds_obj, s=1) {
  xds_obj$L_obj[[s]]$inits
}

#' @title Change initial values (**L**)
#' 
#' @description Change initial values named in `options` for the selected
#' **L** module and vector species, leaving other values unchanged.
#' 
#' @seealso [get_L_inits], [setup_L_inits], [xds_initial_conditions]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$L_obj[[s]]`.
change_L_inits <- function(xds_obj, options=list(), s=1) {
  UseMethod("change_L_inits", xds_obj$L_obj[[s]])
}

#' @title Set up variable indices (**L**)
#' @description Set up variable indices for the selected **L** module and
#' vector species.
#' @seealso [get_L_ix]
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$L_obj[[s]]`.
setup_L_ix <- function(xds_obj, s) {
  UseMethod("setup_L_ix", xds_obj$L_obj[[s]])
}

#' @title Get variable indices (**L**)
#'
#' @description Return the stored variable indices for the selected **L** module
#' and vector species.
#' @note Variable indices are stored at `xds_obj$L_obj[[s]]$ix`.
#' If the field is absent, return `NULL`. An empty list is returned unchanged.
#' @seealso [setup_L_ix]
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a named [list] of variable indices, or `NULL` if the field is absent
#' @keywords internal
#' @export
get_L_ix <- function(xds_obj, s=1) {
  xds_obj$L_obj[[s]]$ix
}

#' @title Parse outputs (**L**)
#' @description Extract the model state outputs for the selected **L** module
#' and return them by name in a list.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' These methods process output trajectories after solving, rather than a
#' single state vector during solving. They return results without modifying
#' the model object; the output-processing pipeline stores the parsed results
#' at `xds_obj$outputs$orbits$L[[s]]`.
#' @param outputs a [matrix] of model state outputs
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return a [list]
#' @keywords internal
#' @export
parse_L_orbits <- function(outputs, xds_obj, s) {
  UseMethod("parse_L_orbits", xds_obj$L_obj[[s]])
}


#' @title Get orbits (**L**)
#'
#' @description Return the saved, parsed orbits (trajectories) for **L**
#' for the selected species after solving.
#' @note Orbits are stored at `xds_obj$outputs$orbits$L[[s]]`.
#' The returned list includes `time` from `xds_obj$outputs$time`.
#' This function does not parse outputs
#' or modify the model object.
#' @seealso [parse_L_orbits], [xds_solve]
#'
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [list] of module-specific orbits (trajectories) with a `time` vector
#'
#' @keywords internal
#' @export
get_L_orbits = function(xds_obj, s=1){

  got = xds_obj$outputs$orbits$L[[s]]
  got$time = xds_obj$outputs$time

  return(got)
}

#' @title Check model (**L**)
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_L = function(xds_obj, s){
  UseMethod("check_L", xds_obj$L_obj[[s]])
}
#' @title Get the skill set (**L**)
#'
#' @description Return the selected module's capabilities and compatibilities as a list.
#' Non-trivial modules include logical `<parameter>_is_port` flags for
#' `psi`, `phi`, `xi`, and `theta`. A flag is `TRUE` when the module implements
#' that parameter as a port, and `FALSE` when it is a direct value or is absent.
#'
#' @note Dispatch is on the class of `xds_obj$L_obj[[s]]`.
#' If a module name is passed instead of a model object, that name selects the method.
#'
#' @param xds_obj an **`xds`** model object (or the module name)
#' @param s the species index
#'
#' @return the **L** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_L = function(xds_obj, s=1){
  if(is.character(xds_obj)){
    name <- xds_obj
    class(name) <- name
  } else {
    name <- xds_obj$L_obj[[s]]
  }
  UseMethod("get_skillset_L", name)
}

#' @title Compute steady states (**L**)
#'
#' @description Compute steady-state aquatic mosquito densities for the selected
#' **L** module given the daily egg input `eta` in each habitat.
#' @note Dispatch on the module object at `xds_obj$L_obj[[s]]`.
#' Equation-system selection and supported steady-state calculations depend
#' on the module. Return values are not stored on the model object.
#'
#' @param eta the number of eggs laid in each habitat, per day
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#'
#' @return a named [list]: values of the state variables at the steady state
#' @keywords internal
#' @export
steady_state_L = function(eta, xds_obj, s=1){
  UseMethod("steady_state_L", xds_obj$L_obj[[s]])
}
