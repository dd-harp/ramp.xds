
#' @title Setup the skill set
#'
#' @description The skill set for an **MY** module
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
setup_skillset_MY = function(xds_obj, s){
  UseMethod("setup_skillset_MY", xds_obj$MY_obj[[s]])
}

#' Run a check before solving
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
check_MY = function(xds_obj, s){
  UseMethod("check_MY", xds_obj$MY_obj[[s]])
}

#' @title Compute derivatives (**MY**)
#'
#' @description
#'
#' In the description, each method should include the equations and
#' a reference to relevant papers in the literature.
#'
#' @note This is the `S3` generic. Methods dispatch on `MY_obj[[s]]`
#'
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return Derivatives for an adult mosquito model, a [vector]
#'
#' @keywords internal
#' @export
dMYdt <- function(t, y, xds_obj, s) {
  UseMethod("dMYdt", xds_obj$MY_obj[[s]])
}

#' @title Update states (**MY**) 
#'
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return Updated states for an adult mosquito model, a [vector]
#'
#' @keywords internal
#' @export
Update_MYt <- function(t, y, xds_obj, s) {
  UseMethod("Update_MYt", xds_obj$MY_obj[[s]])
}


#' @title Blood feeding rate of the infective mosquito population
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqZ <- function(t, y, xds_obj, s) {
  UseMethod("F_fqZ", xds_obj$MY_obj[[s]])
}

#' @title Blood feeding rate of the mosquito population
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_fqM <- function(t, y, xds_obj, s) {
  UseMethod("F_fqM", xds_obj$MY_obj[[s]])
}

#' @title Number of eggs laid by adult mosquitoes
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [numeric] vector of length `nPatches`
#' @keywords internal
#' @export
F_eggs <- function(t, y, xds_obj, s) {
  UseMethod("F_eggs", xds_obj$MY_obj[[s]])
}


#' @title Mosquito bionomics (**MY**)
#' @description Compute the adult mosquito bionomic parameter values
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
MBionomics <- function(t, y, xds_obj, s) {
  UseMethod("MBionomics", xds_obj$MY_obj[[s]])
}

#' @title Apply effect sizes (**MY**)
#' @description Apply vector control effect sizes to adult mosquito bionomic parameters
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
MEffectSizes <- function(t, y, xds_obj, s) {
  UseMethod("MEffectSizes", xds_obj$MY_obj[[s]])
}

#' @title Set up an **MY** model object
#'
#' @description
#' This setup creates an **MY** object for the \eqn{s^{th}} species
#' and sets up the interface ports: 
#' + calls `make_MY_obj_`*MYname* to set up the dynamics, including parameters
#' + calls [setup_MY_inits] to set initial values
#' + calls [setup_F_circadian] to set up a flat circadian function (no effect)
#' + calls [setup_K_matrix] to set up a zero matrix
#' 
#' It also sets up computation for module-specific objects.
#' 
#' The function is part of basic setup, but it can also be used to add vector species. 
#' 
#' @note The object is stored as `xds_obj$MY_obj[[s]]`
#' 
#' @seealso [xds_setup], [change_MY_pars], [setup_MY_inits], [setup_F_circadian], [setup_K_matrix]
#'
#' @param MYname the name of the model
#' @param xds_obj an **`xds`** model object
#' @param options a [list]
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' 
#' @keywords internal
#' @export
setup_MY_obj = function(MYname, xds_obj, options=list(), s=1){
  class(MYname) <- MYname
  UseMethod("setup_MY_obj", MYname)
}

#' @title Add indices for adult mosquitoes to parameter list
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return [list]
#' @keywords internal
#' @export
setup_MY_ix <- function(xds_obj, s) {
  UseMethod("setup_MY_ix", xds_obj$MY_obj[[s]])
}

#' @title List variables (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param y the variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
get_MY_vars <- function(y, xds_obj, s) {
  UseMethod("get_MY_vars", xds_obj$MY_obj[[s]])
}

#' @title Get **MY** variable indices
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return [list]
#' @keywords internal
#' @export
get_MY_ix <- function(xds_obj, s) {
  xds_obj$MY_obj[[s]]$ix
}

#' @title Parse outputs (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' It computes the variables by name and returns a named list.
#' @param outputs a [matrix] of outputs from deSolve
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return [list]
#' @keywords internal
#' @export
parse_MY_orbits <- function(outputs, xds_obj, s) {
  UseMethod("parse_MY_orbits", xds_obj$MY_obj[[s]])
}

#' @title Get parameters (**MY**)
#' 
#' @description 
#' Inspect the parameter values for the **MY** module for the \eqn{s^{th}} species
#' 
#' @seealso [change_MY_pars], [setup_MY_obj], [xds_info_change_parameters]
#' 
#' @note Parameter values are stored on `xds_obj$MY_obj[[s]]`
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return a [list]
#' @keywords internal
#' @export
get_MY_pars <- function(xds_obj, s=1) {
  UseMethod("get_MY_pars", xds_obj$MY_obj[[s]])
}

#' @title Change parameters (**MY**)
#' 
#' @description 
#' For the **MY** module for the \eqn{s^{th}} species, 
#' change parameter values named in `options`,
#' leaving other values unchanged.
#' 
#' @seealso [get_MY_pars], [setup_MY_obj], [xds_info_change_parameters]
#'
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
change_MY_pars <- function(xds_obj, options=list(), s=1) {
  UseMethod("change_MY_pars", xds_obj$MY_obj[[s]])
}

#' @title Setup initial values (**MY**)
#' 
#' @description Set initial values
#' for all the variables.  
#' + Each module (*MYname*) defines
#' a function `make_MY_inits_`*MYname* 
#' that supplies default values
#' + The values in `options` override the defaults
#' + To inspect variable names, use [get_MY_inits] or 
#' see the inline module documentation.
#'
#' @seealso [get_MY_inits], [xds_info_initial_conditions]
#' 
#'
#' @param xds_obj an **`xds`** model object
#' @param options a [list]
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_MY_inits = function(xds_obj, options=list(), s=1){
  UseMethod("setup_MY_inits", xds_obj$MY_obj[[s]])
}


#' @title Get initial values (**MY**) 
#' 
#' @description Inspect the initial values for the **MY** module
#' 
#' @note The initial values are stored as a named list called `xds_obj$MY_obj[[s]]$inits`
#' 
#' @seealso [change_MY_inits], [setup_MY_inits]
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the initial values, as a named list
#' @keywords internal
#' @export
get_MY_inits <- function(xds_obj, s=1) {
  xds_obj$MY_obj[[s]]$inits
}

#' @title Change initial values (**MY**)
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** model object
#' @param options a named list
#' @param s the vector species index
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_MY_inits <- function(xds_obj, options=list(), s=1) {
  UseMethod("change_MY_inits", xds_obj$MY_obj[[s]])
}

#' @title Get the feeding rate(s)
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the feeding rate, as a vector
#' @export
get_f = function(xds_obj, s=1){
  UseMethod("get_f", xds_obj$MY_obj[[s]])
}

#' @title Get the human fraction(s)
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the human fraction, as a vector
#' @export
get_q = function(xds_obj, s=1){
  UseMethod("get_q", xds_obj$MY_obj[[s]])
}

#' @title Get the adult mosquito mortality rate(s)
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the mortality rate, as a vector
#' @export
get_g = function(xds_obj, s=1){
  UseMethod("get_g", xds_obj$MY_obj[[s]])
}

#' @title Get the patch emigration rates
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return the patch emigration rates, as a vector
#' @export
get_sigma = function(xds_obj, s=1){
  UseMethod("get_sigma", xds_obj$MY_obj[[s]])
}

#' @title Get mosquito population density
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_M = function(xds_obj, s=1){
  UseMethod("get_M", xds_obj$MY_obj[[s]])
}

#' @title Get the density of infectious, blood feeding mosquitoes
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @keywords internal
#' @export
get_Z = function(xds_obj, s=1){
  UseMethod("get_Z", xds_obj$MY_obj[[s]])
}

#' @title Compute steady states for **MY**
#'
#' @description
#' Given the emergence rate of adult mosquitoes (\eqn{\Lambda} or `Lambda`) and the
#' net infectiousness of humans (\eqn{\kappa} or `kappa`), compute the steady states
#' for the system
#'
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param kappa net infectiousness
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the steady states
#' @export
steady_state_MY = function(Lambda, kappa, xds_obj, s=1){
  UseMethod("steady_state_MY", xds_obj$MY_obj[[s]])
}

#' @title Compute steady states for **M**
#'
#' @description
#'
#' Given the emergence rate of adult mosquitoes (\eqn{\Lambda} or `Lambda`) compute
#' the population density of adult mosquitoes at the steady state
#'
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the steady states
#'
#' @export
steady_state_M = function(Lambda, xds_obj, s=1){
  UseMethod("steady_state_M", xds_obj$MY_obj[[s]])
}

#' @title Compute steady states for **Y**
#'
#' @description
#' Given adult mosquito population density (\eqn{M} or `M`) and the
#' net infectiousness of humans (\eqn{\kappa} or `kappa`), compute the steady states
#' for the other states
#'
#' @param M adult mosquito population density
#' @param kappa net infectiousness
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return none
#' @export
steady_state_Y = function(M, kappa, xds_obj, s=1){
  UseMethod("steady_state_M", xds_obj$MY_obj[[s]])
}
