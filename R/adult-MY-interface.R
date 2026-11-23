# X
# TODO: Complete the deferred check_MY family description review across modules.

#' @title Compute the derivatives (**MY**)
#'
#' @description
#'
#' Compute the derivatives of the adult mosquito state variables using the
#' selected **MY** module. See the module-specific documentation for equations
#' and references.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [numeric] vector of derivatives in the module's state-variable order
#'
#' @keywords internal
#' @export
dMYdt <- function(t, y, xds_obj, s) {
  UseMethod("dMYdt", xds_obj$MY_obj[[s]])
}

#' @title Update states (**MY**) 
#'
#' @description Compute the updated adult mosquito states for a discrete-time
#' step using the selected **MY** module.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [numeric] vector of updated states in the module's state-variable
#' order; the current RMdts and SEI methods return named lists instead
#'
#' @keywords internal
#' @export
Update_MYt <- function(t, y, xds_obj, s) {
  UseMethod("Update_MYt", xds_obj$MY_obj[[s]])
}

#' @title Get the state variables (**MY**)
#' @description Extract the state variables for the selected **MY** module and vector species
#' from `y` and return them by name in a list for use during model solving.
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of state variables and module-specific derived quantities
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$MY_obj[[s]]`.
#' These functions support solver evaluations that need variables by name;
#' users rarely need to call them directly.
get_MY_vars <- function(y, xds_obj, s) {
  UseMethod("get_MY_vars", xds_obj$MY_obj[[s]])
}


#' @title Mosquito bionomics (**MY**)
#' @description Compute the adult mosquito bionomic parameter values.
#' Store each port-backed parameter `x` as `x_t` and reset `es_x` to 1.
#' Parameters without ports are stored directly as `x`.
#' @seealso [MosquitoBionomics]
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MBionomics <- function(t, y, xds_obj, s) {
  UseMethod("MBionomics", xds_obj$MY_obj[[s]])
}

#' @title Apply effect sizes (**MY**)
#' @description Apply vector control effect sizes to adult mosquito bionomic parameters.
#' For each port-backed parameter, set `x = x_t * es_x`.
#' Leave parameters without ports unchanged.
#' @seealso [VectorControlEffectSizes]
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
MEffectSizes <- function(t, y, xds_obj, s) {
  UseMethod("MEffectSizes", xds_obj$MY_obj[[s]])
}

#' @title Compute infective mosquito biting density (**MY**)
#' @description Return the daily infective mosquito biting density on humans,
#' `fqZ`, for each patch.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
#' @seealso [F_fqM]
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' This is a patch-level biting density, not a per-host biting rate.
F_fqZ <- function(t, y, xds_obj, s) {
  UseMethod("F_fqZ", xds_obj$MY_obj[[s]])
}

#' @title Compute mosquito biting density (**MY**)
#' @description Return the daily mosquito biting density on humans,
#' `fqM`, for each patch.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @seealso [F_hbr], [F_fqZ]
#' @keywords internal
#' @export
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' This is a patch-level biting density, not a per-host biting rate.
F_fqM <- function(t, y, xds_obj, s) {
  UseMethod("F_fqM", xds_obj$MY_obj[[s]])
}

#' @title Compute egg production (**MY**)
#' 
#' @description 
#' Return the number of eggs laid by the adult mosquito population, per patch
#' 
#' @section Egg Laying Rates:
#' The number of eggs laid by the adult mosquito population, per patch, per day
#' is the product of:
#' + \eqn{M} --- the number of egg-laying mosquitoes, per patch
#' + \eqn{\nu} --- the oviposition rate: the number of egg batches laid in each patch, per mosquito, per day
#' + \eqn{\Gamma} --- the number of eggs per batch
#' \deqn{G = \nu M \Gamma}
#' Behavioral-state modules use gravid mosquito density in place of `M`.
#' 
#' @seealso [compute_eggs_laid], [egg_laying], [EggLaying]
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs <- function(t, y, xds_obj, s) {
  UseMethod("F_eggs", xds_obj$MY_obj[[s]])
}

#' @title Get the feeding rate (**MY**)
#' @description Return the stored realized feeding rate per patch.
#' @details Read the realized parameter without evaluating its port or applying
#' effect sizes. Run bionomics and effect-size application to refresh port-backed values.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @export
get_f = function(xds_obj, s=1){
  UseMethod("get_f", xds_obj$MY_obj[[s]])
}

#' @title Get the human blood fraction (**MY**)
#' @description Return the stored realized human blood fraction per patch.
#' @details Read the realized parameter without evaluating its port or applying
#' effect sizes. Run bionomics and effect-size application to refresh port-backed values.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @export
get_q = function(xds_obj, s=1){
  UseMethod("get_q", xds_obj$MY_obj[[s]])
}

#' @title Get the adult mosquito mortality rate (**MY**)
#' @description Return the stored realized adult mosquito mortality rate per patch.
#' @details Read the realized parameter without evaluating its port or applying
#' effect sizes. Run bionomics and effect-size application to refresh port-backed values.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @details The trivialMY method returns an empty numeric vector.
#' @return a [numeric] vector of length `nPatches`; empty for trivialMY
#' @export
get_g = function(xds_obj, s=1){
  UseMethod("get_g", xds_obj$MY_obj[[s]])
}

#' @title Get the patch emigration rate (**MY**)
#' @description Return the stored realized patch emigration rate per patch.
#' @details Read the realized parameter without evaluating its port or applying
#' effect sizes. Run bionomics and effect-size application to refresh port-backed values.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @details Behavioral-state modules BQ, BQm, and BRQS return `sigma_b`.
#' The trivialMY method returns an empty numeric vector.
#' @return a [numeric] vector of length `nPatches`; empty for trivialMY
#' @export
get_sigma = function(xds_obj, s=1){
  UseMethod("get_sigma", xds_obj$MY_obj[[s]])
}

#' @title Set up the module object (**MY**)
#'
#' @description
#' Set up the selected module and its interface ports for the
#' \eqn{s^{th}} vector species in the **MY** component.
#'
#' + Pass `xds_obj`, `options`, and `s` to the selected setup method.
#' + Let that method construct the module object and configure its
#'   initial values and interface ports as appropriate for the module.
#' + Return the updated **`xds`** model object.
#' 
#' The function is part of basic setup, but it can also be used to add vector species. 
#' 
#' @note The object is stored as `xds_obj$MY_obj[[s]]`
#' Dispatch is on `MYname`, whose class is set to the supplied module name,
#' not on an existing module object.
#' 
#' @seealso [xds_setup], [change_MY_pars], [setup_MY_inits], [setup_F_circadian], [setup_K_matrix]
#'
#' @param MYname the name of the model
#' @param xds_obj an **`xds`** model object
#' @param options a [list]
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' 
#' @keywords internal
#' @export
setup_MY_obj = function(MYname, xds_obj, options=list(), s=1){
  class(MYname) <- MYname
  UseMethod("setup_MY_obj", MYname)
}

#' @title Get parameters (**MY**)
#' 
#' @description Return the parameters for the selected **MY** module
#' for the \eqn{s^{th}} vector species as a named list.
#' 
#'  
#' @seealso [change_MY_pars], [setup_MY_obj], [xds_info_change_parameters]
#' 
#' @note Parameter values are stored on `xds_obj$MY_obj[[s]]`.
#' Dispatch is on the class of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of parameter values
#' @keywords internal
#' @export
get_MY_pars <- function(xds_obj, s=1) {
  UseMethod("get_MY_pars", xds_obj$MY_obj[[s]])
}

#' @title Change parameters (**MY**)
#' 
#' @description Change parameter values named in `options` for the selected
#' **MY** module for the \eqn{s^{th}} vector species, leaving other values unchanged.
#' 
#' @note
#' This is designed to work for static parameters or 
#' for parameters that are set up as ports, where the class of
#' the bionomic object is "static." 
#' 
#' Use advanced setup to modify dynamic bionomic ports. 
#' Dispatch is on the class of `xds_obj$MY_obj[[s]]`.
#' 
#' @seealso [get_MY_pars], [setup_MY_obj], [xds_info_change_parameters]
#'
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** model object
#' @export
change_MY_pars <- function(xds_obj, options=list(), s=1) {
  UseMethod("change_MY_pars", xds_obj$MY_obj[[s]])
}

#' @title Set up initial values (**MY**)
#' 
#' @description Set up initial values for the selected **MY** module and
#' vector species. Values in `options` override the defaults.
#' @seealso [get_MY_inits], [xds_info_initial_conditions]
#' 
#'
#' @param xds_obj an **`xds`** model object
#' @param options a [list]
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$MY_obj[[s]]`.
#' @seealso [change_MY_inits]
setup_MY_inits = function(xds_obj, options=list(), s=1){
  UseMethod("setup_MY_inits", xds_obj$MY_obj[[s]])
}


#' @title Get initial values (**MY**)
#' 
#' @description Return the stored initial values for the selected **MY** module
#' and vector species.
#' 
#' @note Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.
#' If the field is absent, as in the `trivialMY` module, return `NULL`.
#' An empty list is returned unchanged.
#' 
#' @seealso [change_MY_inits], [setup_MY_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of initial values, or `NULL` if the field is absent
#' @keywords internal
#' @export
get_MY_inits <- function(xds_obj, s=1) {
  xds_obj$MY_obj[[s]]$inits
}

#' @title Change initial values (**MY**)
#' @description Change initial values named in `options` for the selected
#' **MY** module and vector species, leaving other values unchanged.
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$MY_obj[[s]]`.
change_MY_inits <- function(xds_obj, options=list(), s=1) {
  UseMethod("change_MY_inits", xds_obj$MY_obj[[s]])
}

#' @title Get the initial adult mosquito density (**MY**)
#' @description Return the initial adult mosquito density per patch.
#' @details Return initial density, not current solver states or output trajectories.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @seealso [get_MY_inits], [get_MY_vars], [get_MY_orbits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_M = function(xds_obj, s=1){
  UseMethod("get_M", xds_obj$MY_obj[[s]])
}

#' @title Get the initial infective mosquito density (**MY**)
#' @description Return the initial infective mosquito density per patch.
#' @details Return initial density, not current solver states or output trajectories.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @seealso [get_MY_inits], [get_MY_vars], [get_MY_orbits]
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
get_Z = function(xds_obj, s=1){
  UseMethod("get_Z", xds_obj$MY_obj[[s]])
}

#' @title Set up variable indices (**MY**)
#' @description Set up variable indices for the selected **MY** module and
#' vector species.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$MY_obj[[s]]`.
setup_MY_ix <- function(xds_obj, s) {
  UseMethod("setup_MY_ix", xds_obj$MY_obj[[s]])
}

#' @title Get variable indices (**MY**)
#' @description Return the stored variable indices for the selected **MY** module
#' and vector species.
#' @note Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`.
#' If the field is absent, return `NULL`. An empty list is returned unchanged.
#' @seealso [setup_MY_ix]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a named [list] of variable indices, or `NULL` if the field is absent
#' @keywords internal
#' @export
get_MY_ix <- function(xds_obj, s=1) {
  xds_obj$MY_obj[[s]]$ix
}

#' @title Parse outputs (**MY**)
#' @description Extract the model state outputs for the selected **MY** module
#' and return them by name in a list.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' These methods process output trajectories after solving, rather than a
#' single state vector during solving. They return results without modifying
#' the model object; the output-processing pipeline stores the parsed results
#' at `xds_obj$outputs$orbits$MY[[s]]`.
#' @param outputs a [matrix] of model state outputs
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
parse_MY_orbits <- function(outputs, xds_obj, s) {
  UseMethod("parse_MY_orbits", xds_obj$MY_obj[[s]])
}

#' @title Check model (**MY**)
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY = function(xds_obj, s){
  UseMethod("check_MY", xds_obj$MY_obj[[s]])
}
#' @title Get the skill set (**MY**)
#'
#' @description Return the selected module's capabilities and compatibilities as a list.
#' Non-trivial modules include logical `<parameter>_is_port` flags for
#' `f`, `q`, `g`, `sigma`, `sigma_b`, `sigma_q`, `mu`, `nu`, `eip`, and `K_matrix`.
#' A flag is `TRUE` when the module implements that parameter as a port,
#' and `FALSE` when the parameter is a direct value or is absent.
#'
#' @note Dispatch is on the class of `xds_obj$MY_obj[[s]]`.
#' If a module name is passed instead of a model object, that name selects the method.
#'
#' @param xds_obj an **`xds`** model object (or the module name)
#' @param s the species index
#'
#' @return the **MY** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_MY = function(xds_obj, s=1){
  if(is.character(xds_obj)){
    name <- xds_obj
    class(name) <- name
  } else {
    name <- xds_obj$MY_obj[[s]]
  }
  UseMethod("get_skillset_MY", name)
}

#' @title Compute steady states (**M**)
#'
#' @description Compute steady-state mosquito densities for the selected **M**
#' component given adult mosquito emergence `Lambda`.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' Equation-system selection and supported steady-state calculations depend
#' on the module. Return values are not stored on the model object.
#'
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return module-specific steady-state values; see the selected method for
#' the return type and state names
#'
#' @export
steady_state_M = function(Lambda, xds_obj, s=1){
  UseMethod("steady_state_M", xds_obj$MY_obj[[s]])
}

#' @title Compute steady states (**MY**)
#'
#' @description Compute steady-state mosquito densities for the selected **MY**
#' module given adult mosquito emergence `Lambda` and net infectiousness `kappa`.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' Equation-system selection and supported steady-state calculations depend
#' on the module. Return values are not stored on the model object.
#'
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param kappa net infectiousness
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return module-specific steady-state values; see the selected method for
#' the return type and state names
#' @export
steady_state_MY = function(Lambda, kappa, xds_obj, s=1){
  UseMethod("steady_state_MY", xds_obj$MY_obj[[s]])
}

#' @title Compute steady states (**Y**)
#'
#' @description Compute steady-state infection densities for the selected **Y**
#' component given adult mosquito density `M` and net infectiousness `kappa`.
#' @note Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
#' A module-specific method must be supplied to use this generic.
#' Return values are not stored on the model object.
#'
#' @param M adult mosquito population density
#' @param kappa net infectiousness
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return module-specific steady-state values; see the selected method for
#' the return type and state names
#' @export
steady_state_Y = function(M, kappa, xds_obj, s=1){
  UseMethod("steady_state_Y", xds_obj$MY_obj[[s]])
}
