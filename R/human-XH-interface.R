# X
# TODO: Complete the deferred check_XH family description review across modules.
# TODO: Complete the discrete-time update equation-description pass across XH
# modules after resolving the flagged implementation issues.

# generic methods for human component


#' @title Compute the derivatives (**XH**)
#'
#' @description Using the stored values
#' of the daily FoI, compute the derivatives and
#' return the derivatives as a numeric vector.
#'
#' @note This method dispatches on `class(xds_obj$XH_obj[[i]])`.
#'
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return the derivatives as a [numeric] vector
#'
#' @keywords internal
#' @export
dXHdt <- function(t, y, xds_obj, i) {
  UseMethod("dXHdt", xds_obj$XH_obj[[i]])
}

#' @title Update states (**XH**)
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_XHt <- function(t, y, xds_obj, i) {
  UseMethod("Update_XHt", xds_obj$XH_obj[[i]])
}

#' @title Get the state variables (**XH**)
#'
#' @description Extract the state variables for the selected **XH** module and host species
#' from `y` and return them by name in a list for use during model solving.
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a named [list] of state variables and module-specific derived quantities
#'
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$XH_obj[[i]]`.
#' These functions support solver evaluations that need variables by name;
#' users rarely need to call them directly.
get_XH_vars <- function(y, xds_obj, i=1) {
  UseMethod("get_XH_vars", xds_obj$XH_obj[[i]])
}



#' @title Size of effective infectious human population
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_I <- function(t, y, xds_obj, i) {
  UseMethod("F_I", xds_obj$XH_obj[[i]])
}

#' @title Size of human population denominators
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_H <- function(t, y, xds_obj, i) {
  UseMethod("F_H", xds_obj$XH_obj[[i]])
}

#' @title Infection blocking pre-erythrocytic immunity
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_infectivity <- function(y, xds_obj, i) {
  UseMethod("F_infectivity", xds_obj$XH_obj[[i]])
}

#' @title Set up the module object (**XH**)
#' @description 
#' Set up the selected module and its interface ports for the
#' \eqn{i^{th}} host species in the **XH** component.
#'
#' + Pass `residence`, `xds_obj`, `options`, and `i` to the selected setup method.
#' + Let that method construct the module object and configure its
#'   initial values, exposure, travel, demography, and treatment interfaces
#'   as appropriate for the module.
#' + Return the updated **`xds`** model object.
#' 
#' The function is part of basic setup, but it is also used to add a host species. 
#' 
#' @seealso [xds_setup], [change_XH_pars], [setup_XH_inits], [setup_timespent], [setup_mass_treatment], 
#' [setup_demog_obj], [setup_time_away], [setup_travel_eir]
#' 
#' @note The module object is stored as `xds_obj$XH_obj[[i]]`.
#' Dispatch is on `XHname`, whose class is set to the supplied module name,
#' not on an existing module object.
#' 
#' @param XHname the model name
#' @param residence the residence vector
#' @param xds_obj an **`xds`** model object
#' @param options model options as a named list
#' @param i the host species index
#'
#' @return an **`xds`** model object
#' 
#' @keywords internal
#'
#' @export
setup_XH_obj = function(XHname, residence, xds_obj, options=list(), i=1){
  class(XHname) <- XHname
  UseMethod("setup_XH_obj", XHname)
}


#' @title Get parameters (**XH**)
#' 
#' @description Return the parameters for the selected **XH** module
#' for the \eqn{i^{th}} host species as a named list.
#' 
#' @seealso [change_XH_pars], [setup_XH_obj], [xds_info_change_parameters]
#' 
#' @note The parameter values are stored on `xds_obj$XH_obj[[i]]`.
#' Dispatch is on the class of `xds_obj$XH_obj[[i]]`.
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a named [list] of parameter values
#' @keywords internal
#' @export
get_XH_pars <- function(xds_obj, i=1) {
  UseMethod("get_XH_pars", xds_obj$XH_obj[[i]])
}

#' @title Change parameters (**XH**)
#' 
#' @description Change parameter values named in `options` for the selected
#' **XH** module for the \eqn{i^{th}} host species, leaving other values unchanged.
#'
#' @note Dispatch is on the class of `xds_obj$XH_obj[[i]]`.
#' 
#' @seealso [get_XH_pars], [setup_XH_obj], [xds_info_change_parameters]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param i the host species index
#' 
#' @return an **`xds`** model object
#' 
#' @keywords internal
#' @export
change_XH_pars <- function(xds_obj, options=list(), i=1) {
  UseMethod("change_XH_pars", xds_obj$XH_obj[[i]])
}

#' @title Set up initial values (**XH**)
#'
#' @description Set up initial values for the selected **XH** module and
#' host species. Values in `options` override the defaults.
#' @seealso [get_XH_inits], [xds_info_initial_conditions]
#' @param xds_obj an **`xds`** model object
#' @param options a [list]
#' @param i the host species index
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$XH_obj[[i]]`.
#' @seealso [change_XH_inits]
setup_XH_inits = function(xds_obj, options=list(), i=1){
  UseMethod("setup_XH_inits", xds_obj$XH_obj[[i]])
}


#' @title Get initial values (**XH**)
#'
#' @description Return the stored initial values for the selected **XH** module
#' and host species.
#'
#' @note Initial values are stored at `xds_obj$XH_obj[[i]]$inits`.
#' If the field is absent, as in the `trivialXH` module, return `NULL`.
#' An empty list is returned unchanged.
#' 
#' @seealso [change_XH_inits], [setup_XH_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a named [list] of initial values, or `NULL` if the field is absent
#'
#' @keywords internal
#' @export
get_XH_inits = function(xds_obj, i=1){
  xds_obj$XH_obj[[i]]$inits
}


#' @title Change initial values (**XH**)
#' 
#' @description Change initial values named in `options` for the selected
#' **XH** module and host species, leaving other values unchanged.
#' 
#' @seealso [get_XH_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param i the host species index
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$XH_obj[[i]]`.
change_XH_inits <- function(xds_obj, options=list(), i=1) {
  UseMethod("change_XH_inits", xds_obj$XH_obj[[i]])
}


#' @title Set up variable indices (**XH**)
#' @description Set up variable indices for the selected **XH** module and
#' host species.
#'
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return an **`xds`** model object
#' @keywords internal
#' @export
#' @note Dispatch uses the class of `xds_obj$XH_obj[[i]]`.
setup_XH_ix <- function(xds_obj, i) {
  UseMethod("setup_XH_ix", xds_obj$XH_obj[[i]])
}

#' @title Get variable indices (**XH**)
#'
#' @description Return the stored variable indices for the selected **XH** module
#' and host species.
#'
#' @note Variable indices are stored at `xds_obj$XH_obj[[i]]$ix`.
#' If the field is absent, return `NULL`. An empty list is returned unchanged.
#' @seealso [setup_XH_ix]
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a named [list] of variable indices, or `NULL` if the field is absent
#' @keywords internal
#' @export
get_XH_ix <- function(xds_obj, i=1) {
  xds_obj$XH_obj[[i]]$ix
}


#' @title Parse outputs (**XH**)
#'
#' @description Extract the model state outputs for the selected **XH** module
#' and return them by name in a list.
#' @note Dispatch on the module object at `xds_obj$XH_obj[[i]]`.
#' These methods process output trajectories after solving, rather than a
#' single state vector during solving. They return results without modifying
#' the model object; the output-processing pipeline stores the parsed results
#' at `xds_obj$outputs$orbits$XH[[i]]`.
#'
#' @param outputs a [matrix] of model state outputs
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [list]
#' @keywords internal
#' @export
parse_XH_orbits <- function(outputs, xds_obj, i) {
  UseMethod("parse_XH_orbits", xds_obj$XH_obj[[i]])
}

#' @title Compute true prevalance
#' @description A function that translates the state variables into
#' the "true" *Pf*PR
#' @param vars a list with the variables attached by name
#' @param XH_obj a list defining a model for humans
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_prevalence <- function(vars, XH_obj) {
  UseMethod("F_prevalence", XH_obj)
}


#' @title Compute net infectiousness (NI)
#' @description A function to compute NI as an output
#' @param vars a list with the variables attached by name
#' @param XH_obj a list defining a model for humans
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_ni <- function(vars, XH_obj) {
  UseMethod("F_ni", XH_obj)
}


#' @title Compute the human transmitting capacity
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [list]
#' @keywords internal
#' @export
get_HTC <- function(xds_obj, i) {
  UseMethod("get_HTC", xds_obj$XH_obj[[i]])
}

#' @title Compute observed prevalence
#' @description Translate the state variables into predicted observed prevalence.
#' @note Dispatch on the class of `XH_obj`.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pr_obs <- function(vars, XH_obj) {
  UseMethod("F_pr_obs", XH_obj)
}

#' @title Compute observed prevalence by light microscopy
#' @note This method dispatches on the type of `XH_obj`.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_lm <- function(vars, XH_obj) {
  UseMethod("F_pfpr_by_lm", XH_obj)
}

#' @title Compute observed prevalence by RDT
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by rapid diagnostic test (RDT)
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_rdt <- function(vars, XH_obj) {
  UseMethod("F_pfpr_by_rdt", XH_obj)
}

#' @title Compute observed prevalence by PCR
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by PCR
#' @note This method dispatches on the type of `XH_obj`.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_pcr <- function(vars, XH_obj) {
  UseMethod("F_pfpr_by_pcr", XH_obj)
}

#' @title Check model (**XH**)
#'
#' @param xds_obj an **`xds`** model object
#' @param t a vector of times
#' @param i host species index
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_XH = function(xds_obj, t=0, i){
  UseMethod("check_XH", xds_obj$XH_obj[[i]])
}
#' @title Get the skill set (**XH**)
#'
#' @description Return the selected module's capabilities and compatibilities as a list.
#'
#' @note Dispatch is on the class of `xds_obj$XH_obj[[i]]`.
#' If a module name is passed instead of a model object, that name selects the method.
#'
#' @param xds_obj an **`xds`** model object (or the module name)
#' @param i the species index
#'
#' @return the **XH** module skill set, as a list
#'
#' @keywords internal
#' @export
get_skillset_XH = function(xds_obj, i=1){
  if(is.character(xds_obj)){
    name <- xds_obj
    class(name) <- name
  } else {
    name <- xds_obj$XH_obj[[i]]
  }
  UseMethod("get_skillset_XH", name)
}

#' @title Compute steady states (**X**)
#'
#' @description Compute steady-state infection densities for the selected **X**
#' component given daily FoI `foi` and fixed host population density `H`.
#' @note Dispatch on the module object at `xds_obj$XH_obj[[i]]`.
#' Modules with equation-system-specific calculations delegate to a second
#' generic that dispatches on `xds_obj$xds`, selecting `xde` or `dts`.
#' Supported calculations and assumptions depend on the module.
#' Return values are not stored on the model object.
#'
#' @param foi the daily FoI
#' @param H human / host population density
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return module-specific steady-state values as a named [list] or [numeric]
#' vector; see the selected method for state names
#' @keywords internal
#' @export
steady_state_X = function(foi, H, xds_obj, i=1){
  UseMethod("steady_state_X", xds_obj$XH_obj[[i]])
}

#' @title Compute steady states (**XH**)
#'
#' @description Compute steady-state host and infection densities for the
#' selected **XH** module given daily FoI `foi`.
#' @note Dispatch on the module object at `xds_obj$XH_obj[[i]]`.
#' A module-specific method must be supplied to use this generic.
#' Return values are not stored on the model object.
#'
#' @param foi the daily FoI
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return module-specific steady-state values; see the selected method for
#' the return type and state names
#' @keywords internal
#' @export
steady_state_XH = function(foi, xds_obj, i=1){
  UseMethod("steady_state_XH", xds_obj$XH_obj[[i]])
}

#' @title Default plotting for epidemiology (**X**)
#'
#' @description Each module can define a plotting method
#' for the epidemiology
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add plot axes only if FALSE
#'
#' @keywords internal
#' @export
xds_plot_X = function(xds_obj, i=1, clrs="black", llty=1, add=FALSE){
  UseMethod("xds_plot_X", xds_obj$XH_obj[[i]])
}
