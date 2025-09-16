# generic methods for adult component

#' @title The **MY** Module Skill Set 
#' 
#' @description The **MY** skill set is a list of 
#' an module's capabilities: 
#' 
#' + `demography` is 
#'
#' @param MYname  the **MY** module name
#' 
#' @return *MY* module skill set, as a list 
#' 
#' @export
skill_set_MY = function(MYname){
  class(MYname) <- MYname
  UseMethod("skill_set_MY", MYname)
}

#' Run a check before solving 
#'
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index 
#'
#' @returns an **`xds`** model object 
#' @export
check_MY = function(xds_obj, s){
  UseMethod("check_MY", xds_obj$MY_obj[[s]]) 
}

#' @title Compute Derivatives for an Adult Mosquito Model 
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
#' @param xds_obj an `xds` object
#' @param s the species index
#' 
#' @return Derivatives for an adult mosquito model, a [vector]
#' 
#' @export
dMYdt <- function(t, y, xds_obj, s) {
  UseMethod("dMYdt", xds_obj$MY_obj[[s]])
}

#' @title Update States for a Adult Mosquito Model 
#' 
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj a [list]
#' @param s the species index
#' 
#' @return Updated states for an adult mosquito model, a [vector]
#' 
#' @export
Update_MYt <- function(t, y, xds_obj, s) {
  UseMethod("Update_MYt", xds_obj$MY_obj[[s]])
}


#' @title Blood feeding rate of the infective mosquito population
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ <- function(t, y, xds_obj, s) {
  UseMethod("F_fqZ", xds_obj$MY_obj[[s]])
}

#' @title Blood feeding rate of the mosquito population
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM <- function(t, y, xds_obj, s) {
  UseMethod("F_fqM", xds_obj$MY_obj[[s]])
}

#' @title Number of eggs laid by adult mosquitoes
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs <- function(t, y, xds_obj, s) {
  UseMethod("F_eggs", xds_obj$MY_obj[[s]])
}


#' @title Adult Mosquito Bionomics - Baseline 
#' @description Compute the baseline adult mosquito bionomic parameter values 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
MBaseline <- function(t, y, xds_obj, s) {
  UseMethod("MBaseline", xds_obj$MY_obj[[s]])
}

#' @title Adult Mosquito Bionomics - Modified by Control
#' @description Modify the baseline adult mosquito bionomic parameters
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
MBionomics <- function(t, y, xds_obj, s) {
  UseMethod("MBionomics", xds_obj$MY_obj[[s]])
}

#' @title Setup an **MY** Module (Adult Mosquito Ecology & Infection Dynamics) 
#' @description This method dispatches on `MYname`.
#' @param MYname the name of the model
#' @param xds_obj a [list]
#' @param s the species index
#' @param options a [list]
#' @return [list]
#' @export
setup_MY_obj = function(MYname, xds_obj, s, options=list()){
  class(MYname) <- MYname
  UseMethod("setup_MY_obj", MYname)
}

#' @title Add indices for adult mosquitoes to parameter list
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param xds_obj a [list]
#' @param s the species index
#' @return [list]
#' @export
setup_MY_ix <- function(xds_obj, s) {
  UseMethod("setup_MY_ix", xds_obj$MY_obj[[s]])
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param y the variables
#' @param xds_obj a [list]
#' @param s the vector species index
#' @return a [list]
#' @export
get_MY_vars <- function(y, xds_obj, s) {
  UseMethod("get_MY_vars", xds_obj$MY_obj[[s]])
}

#' @title Get **MY** Variable Indices
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param xds_obj a [list]
#' @param s the species index
#' @return [list]
#' @export
get_MY_ix <- function(xds_obj, s) {
  xds_obj$MY_obj[[s]]$ix
}

#' @title parse the outputs and return the variables by name in a list
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' It computes the variables by name and returns a named list.
#' @param outputs a [matrix] of outputs from deSolve
#' @param xds_obj a [list] that defines a model
#' @param s the species index
#' @return [list]
#' @export
parse_MY_orbits <- function(outputs, xds_obj, s) {
  UseMethod("parse_MY_orbits", xds_obj$MY_obj[[s]])
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MY_pars <- function(xds_obj, s=1) {
  UseMethod("get_MY_pars", xds_obj$MY_obj[[s]])
}

#' @title Set new MY parameter values
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_MY_pars <- function(xds_obj, s=1, options=list()) {
  UseMethod("change_MY_pars", xds_obj$MY_obj[[s]])
}

#' @title A function to set up adult mosquito models
#' @description This method dispatches on `MYname`.
#' @param xds_obj a [list]
#' @param s the species index
#' @param options a [list]
#' @return [list]
#' @export
setup_MY_inits = function(xds_obj, s, options=list()){
  UseMethod("setup_MY_inits", xds_obj$MY_obj[[s]])
}


#' @title Return initial values as a vector
#' @description This method dispatches on the type of `xds_obj$MY_obj`.
#' @param xds_obj a [list]
#' @param s the species index
#' @return [numeric]
#' @export
get_MY_inits <- function(xds_obj, s=1) {
  xds_obj$MY_obj[[s]]$inits
}

#' @title Set new MY parameter values
#' @description This method dispatches on the type of `xds_obj$MY_obj[[s]]`.
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @param options a named list
#' @return an `xds` object
#' @export
change_MY_inits <- function(xds_obj, s=1, options=list()) {
  UseMethod("change_MY_inits", xds_obj$MY_obj[[s]])
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f = function(xds_obj, s=1){
  UseMethod("get_f", xds_obj$MY_obj[[s]])
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q = function(xds_obj, s=1){
  UseMethod("get_q", xds_obj$MY_obj[[s]])
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g = function(xds_obj, s=1){
  UseMethod("get_g", xds_obj$MY_obj[[s]])
}

#' @title Get the feeding rate
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma = function(xds_obj, s=1){
  UseMethod("get_sigma", xds_obj$MY_obj[[s]])
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
#' @param MY_obj a list that defines an adult model
#'
#' @return the steady states
#' @export
steady_state_MY = function(Lambda, kappa, MY_obj){
  UseMethod("steady_state_MY", MY_obj)
}

#' @title Compute steady states for **M** 
#' 
#' @description 
#'  
#' Given the emergence rate of adult mosquitoes (\eqn{\Lambda} or `Lambda`) compute 
#' the population density of adult mosquitoes at the steady state
#' 
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param MY_obj a list that defines an adult model
#'
#' @return the steady states
#'  
#' @export
steady_state_M = function(Lambda, MY_obj){
  UseMethod("steady_state_M", MY_obj)
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
#' @param MY_obj a list that defines an adult model
#' 
#' @return none
#' @export
steady_state_Y = function(M, kappa, MY_obj){
  UseMethod("steady_state_M", MY_obj)
}

