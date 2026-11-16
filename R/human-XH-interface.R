# generic methods for human component

#' @title **XH** skill set
#'
#' @description The **XH** skill set is a list of
#' a module's capabilities:
#'
#' + **`H_dynamics`** - if FALSE, \eqn{H} is static. The module doesn't have a port for demographic change
#' + **`mda`**:  if FALSE, the module doesn't have an `mda` port for mass drug administration
#' + **`msat`**:  if FALSE, the module doesn't have an `msat` port for mass
#' + **`malaria`**: if FALSE, the model probably shouldn't be used for malaria
#' + **`pr_obs`**: if TRUE, the model outputs a value for observed PR
#' + **`pf_rdt`**: if TRUE, the model outputs a value for prevalence by RDT
#' + **`pf_lm`**: if TRUE, the model outputs a value for prevalence by light microscopy
#' + **`pf_pcr`**: if TRUE, the model outputs a value for prevalence by PCR
#'
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#' 
#'
#' @return *XH* module skill set, as a list
#'
#' @keywords internal
#' @export
setup_skillset_XH = function(xds_obj, i){
  UseMethod("setup_skillset_XH", xds_obj$XH_obj[[i]])
}

#' @title Run checks before solving (**XH**) 
#'
#' @param xds_obj an **`xds`** model object
#' @param i host species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_XH = function(xds_obj,i){
  UseMethod("check_XH", xds_obj$XH_obj[[i]])
}

#' @title Compute derivatives (**XH**) 
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
#' @keywords internal
#' @export
dXHdt <- function(t, y, xds_obj, i) {
  UseMethod("dXHdt", xds_obj$XH_obj[[i]])
}

#' @title Update state variables (**XH**) 
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector
#' @keywords internal
#' @export
Update_XHt <- function(t, y, xds_obj, i) {
  UseMethod("Update_XHt", xds_obj$XH_obj[[i]])
}



#' @title Size of effective infectious human population
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_I <- function(t, y, xds_obj, i) {
  UseMethod("F_I", xds_obj$XH_obj[[i]])
}

#' @title Size of human population denominators
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_H <- function(t, y, xds_obj, i) {
  UseMethod("F_H", xds_obj$XH_obj[[i]])
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_infectivity <- function(y, xds_obj, i) {
  UseMethod("F_infectivity", xds_obj$XH_obj[[i]])
}

#' @title Set up an **XH** model object
#' @description 
#' This setup creates an **XH** object for the \eqn{i^{th}} species
#' and sets up the interface ports: 
#' + calls `make_XH_obj_`*XHname* to set up the dynamics, including parameters
#' + calls [setup_XH_inits] to set initial values
#' + initializes [setup_blood_search_weights]: all weights are set to \eqn{1}
#' + initializes [setup_timespent] to the residence matrix: all time spent at home
#' + calls [setup_mass_treatment] with no events
#' + calls [setup_demog_obj] setting birth and death rates to zero
#' + calls [setup_time_away] with no time spent away
#' + calls [setup_travel_eir] with the travel EIR set to zero
#' 
#' The function is part of basic setup, but it is also used to add a host species. 
#' 
#' @seealso [xds_setup], [change_XH_pars], [setup_XH_inits], [setup_timespent], [setup_mass_treatment], 
#' [setup_demog_obj], [setup_time_away], [setup_travel_eir]
#' 
#' @note The model object is stored as `xds_obj$XH_obj[[i]]` 
#' 
#' @param XHname the model name
#' @param residence the residence vector
#' @param xds_obj an **`xds`** model object
#' @param options model options as a named list
#' @param i the host species index
#'
#' @return an **`xds`** object
#' 
#' @keywords internal
#'
#' @export
setup_XH_obj = function(XHname, residence, xds_obj, options=list(), i=1){
  class(XHname) <- XHname
  UseMethod("setup_XH_obj", XHname)
}

#' @title List variables (**XH**)
#'
#' @description Get the values
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
#' @keywords internal
#' @export
get_XH_vars <- function(y, xds_obj, i=1) {
  UseMethod("get_XH_vars", xds_obj$XH_obj[[i]])
}


#' @title Add indices for human population to parameter list
#'
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_XH_ix <- function(xds_obj, i) {
  UseMethod("setup_XH_ix", xds_obj$XH_obj[[i]])
}

#' @title Get XH indices
#'
#' @description Get and display the values of the indices
#' for the variables
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
get_XH_ix <- function(xds_obj, i=1) {
  xds_obj$XH_obj[[i]]$ix
}


#' @title Parse outputs (**XH**)
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
#' @return a named [list] of parsed output variables
#' @keywords internal
#' @export
parse_XH_orbits <- function(outputs, xds_obj, i) {
  UseMethod("parse_XH_orbits", xds_obj$XH_obj[[i]])
}


#' @title Get parameters (**XH**)
#' 
#' @description 
#' Inspect the parameter values for the **XH** module for the \eqn{i^{th}} species 
#' 
#' @seealso [change_XH_pars], [setup_XH_obj], [xds_info_change_parameters]
#' 
#' @note The parameter values are stored on `xds_obj$XH_obj[[i]]`
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [list]
#' @keywords internal
#' @export
get_XH_pars <- function(xds_obj, i=1) {
  UseMethod("get_XH_pars", xds_obj$XH_obj[[i]])
}

#' @title Change parameters (**XH**)
#' 
#' @description 
#' For the **XH** module for the \eqn{i^{th}} species, 
#' change parameter values named in `options`,
#' leaving other values unchanged.
#' 
#' @seealso [get_XH_pars], [setup_XH_obj], [xds_info_change_parameters]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param i the host species index
#' 
#' @return an **`xds`** object
#' 
#' @keywords internal
#' @export
change_XH_pars <- function(xds_obj, options=list(), i=1) {
  UseMethod("change_XH_pars", xds_obj$XH_obj[[i]])
}

#' @title Setup initial values (**XH**)
#'
#' @description Set initial values
#' for all the variables.  
#' + Each module (*XHname*) defines
#' a function `make_XH_inits_`*XHname* 
#' that supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_XH_inits] or 
#' see the inline module documentation.
#'
#' @seealso [get_XH_inits], [xds_info_initial_conditions]
#' @param xds_obj an **`xds`** model object
#' @param options a [list]
#' @param i the host species index
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_XH_inits = function(xds_obj, options=list(), i=1){
  UseMethod("setup_XH_inits", xds_obj$XH_obj[[i]])
}


#' @title Get initial values (**XH**)
#'
#' @description Inspect the initial values
#' for the **XH** module
#'
#' @note 
#' The initial values are stored as a named list called `xds_obj$XH_obj[[i]]$inits`
#' 
#' @seealso [change_XH_inits], [setup_XH_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return the initial values, as a named list
#'
#' @keywords internal
#' @export
get_XH_inits = function(xds_obj, i=1){
  xds_obj$XH_obj[[i]]$inits
}


#' @title Change initial values (**XH**)
#' 
#' @description Change the stored initial values for 
#' any variables named in `options`, leaving other values unchanged.
#' 
#' @seealso [get_XH_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param i the host species index
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_XH_inits <- function(xds_obj, options=list(), i=1) {
  UseMethod("change_XH_inits", xds_obj$XH_obj[[i]])
}


#' @title Compute net infectiousness (NI)
#' @description A function to compute NI as an output
#' @param vars a list with the variables attached by name
#' @param XH_obj a list defining a model for human
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
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
#' @keywords internal
#' @export
F_prevalence <- function(vars, XH_obj) {
  UseMethod("F_prevalence", XH_obj)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_lm <- function(vars, XH_obj) {
  UseMethod("F_pfpr_by_lm", XH_obj)
}

#' @title Compute the prevalence of infection by RDT
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by rapid diagnostic test (RDT)
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_rdt <- function(vars, XH_obj) {
  UseMethod("F_pfpr_by_rdt", XH_obj)
}

#' @title Compute infection prevalence by PCR
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by PCR
#' @note This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @keywords internal
#' @export
F_pfpr_by_pcr <- function(vars, XH_obj) {
  UseMethod("F_pfpr_by_pcr", XH_obj)
}


#' @title Compute the human transmitting capacity
#' @description This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return a [list]
#' @keywords internal
#' @export
get_HTC <- function(xds_obj, i) {
  UseMethod("get_HTC", xds_obj$XH_obj[[i]])
}

#' @title Steady states for **X**
#'
#' @description Compute the steady states as a function of the daily FoI for a
#' static value of human population density
#'
#' @param foi the daily FoI
#' @param H human / host population density
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return steady states
#' @keywords internal
#' @export
steady_state_X = function(foi, H, xds_obj, i=1){
  UseMethod("steady_state_X", xds_obj$XH_obj[[i]])
}

#' @title Steady states for **XH**
#'
#' @description Compute the steady states as a function of the daily FoI for a
#' static value of human population density
#'
#' @param foi the daily FoI
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return steady states
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
