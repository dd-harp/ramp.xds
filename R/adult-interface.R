# generic methods for adult component

#' @title Compute Derivatives for an Adult Mosquito Model 
#' 
#' @description
#' 
#' In the description, each method should include the equations and 
#' a reference to relevant papers in the literature.  
#'
#' @note This is the `S3` generic. Methods dispatch on `MYZpar[[s]]`
#' 
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' 
#' @return Derivatives for an adult mosquito model, a [vector]
#' 
#' @export
dMYZdt <- function(t, y, pars, s) {
  UseMethod("dMYZdt", pars$MYZpar[[s]])
}

#' @title Update States for a Adult Mosquito Model 
#' 
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' 
#' @return Updated states for an adult mosquito model, a [vector]
#' 
#' @export
Update_MYZt <- function(t, y, pars, s) {
  UseMethod("Update_MYZt", pars$MYZpar[[s]])
}

#' @title Adult Mosquito Bionomics - Baseline 
#' @description Compute the baseline adult mosquito bionomic parameter values 
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
MBaseline <- function(t, y, pars, s) {
  UseMethod("MBaseline", pars$MYZpar[[s]]$baseline)
}

#' @title Adult Mosquito Bionomics - Modified by Control
#' @description Modify the baseline adult mosquito bionomic parameters
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
MBionomics <- function(t, y, pars, s) {
  UseMethod("MBionomics", pars$MYZpar[[s]]$baseline)
}

#' @title A function to set up adult mosquito models
#' @description This method dispatches on `MYZname`.
#' @param MYZname the name of the model
#' @param pars a [list]
#' @param s the species index
#' @param MYZopts a [list]
#' @return [list]
#' @export
setup_MYZpar = function(MYZname, pars, s, MYZopts=list()){
  class(MYZname) <- MYZname
  UseMethod("setup_MYZpar", MYZname)
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_MYZpars <- function(pars, s=1) {
  UseMethod("get_MYZpars", pars$MYZpar[[s]])
}

#' @title Set new MYZ parameter values
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param MYZopts a named list
#' @return an `xds` object
#' @export
set_MYZpars <- function(pars, s=1, MYZopts=list()) {
  UseMethod("set_MYZpars", pars$MYZpar[[s]])
}

#' @title Compute probabilities from rates
#' @description This method dispatches on `MYZname`.
#' @param MYZpar the `MYZ` model object
#' @param runtime the model `runtime` parameters
#' @return a [list] with baseline values
#' @export
MYZ_rates2probs = function(MYZpar, runtime){
  UseMethod("MYZ_rates2probs", MYZpar)
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams xde_steady_state_MYZ
#' @return none
#' @export
dts_steady_state_MYZ = function(Lambda, kappa, MYZpar){
  UseMethod("dts_steady_state_MYZ", MYZpar)
}

#' @title Blood feeding rate of the infective mosquito population
#' @description This method dispatches on the type of `pars$MYZpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqZ <- function(t, y, pars, s) {
  UseMethod("F_fqZ", pars$MYZpar[[s]])
}

#' @title Blood feeding rate of the mosquito population
#' @description This method dispatches on the type of `pars$MYZpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nPatches`
#' @export
F_fqM <- function(t, y, pars, s) {
  UseMethod("F_fqM", pars$MYZpar[[s]])
}

#' @title Number of eggs laid by adult mosquitoes
#' @description This method dispatches on the type of `pars$MYZpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eggs <- function(t, y, pars, s) {
  UseMethod("F_eggs", pars$MYZpar[[s]])
}



#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param y the variables
#' @param pars a [list]
#' @param s the vector species index
#' @return a [list]
#' @export
list_MYZvars <- function(y, pars, s) {
  UseMethod("list_MYZvars", pars$MYZpar[[s]])
}

#' @title Put MYZvars in place of the MYZ variables in y
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param MYZvars the variables
#' @param y the variables
#' @param pars a [list]
#' @param s the vector species index
#' @return a [list]
#' @export
put_MYZvars <- function(MYZvars, y, pars, s) {
  UseMethod("put_MYZvars", pars$MYZpar[[s]])
}


#' @title A function to set up adult mosquito models
#' @description This method dispatches on `MYZname`.
#' @param pars a [list]
#' @param s the species index
#' @param MYZopts a [list]
#' @return [list]
#' @export
setup_MYZinits = function(pars, s, MYZopts=list()){
  UseMethod("setup_MYZinits", pars$MYZpar[[s]])
}

#' @title Set new MYZ parameter values
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param MYZopts a named list
#' @return an `xds` object
#' @export
set_MYZinits <- function(pars, s=1, MYZopts=list()) {
  UseMethod("set_MYZinits", pars$MYZpar[[s]])
}

#' @title Add indices for adult mosquitoes to parameter list
#' @description This method dispatches on the type of `pars$MYZpar`.
#' @param pars a [list]
#' @param s the species index
#' @return [list]
#' @export
setup_MYZix <- function(pars, s) {
  UseMethod("setup_MYZix", pars$MYZpar[[s]])
}

#' @title Parse the outputs and return the variables by name in a list
#' @description This method dispatches on the type of `pars$MYZpar`.
#' It computes the variables by name and returns a named list.
#' @param outputs a [matrix] of outputs from deSolve
#' @param pars a [list] that defines a model
#' @param s the species index
#' @return [list]
#' @export
parse_MYZorbits <- function(outputs, pars, s) {
  UseMethod("parse_MYZorbits", pars$MYZpar[[s]])
}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$MYZpar`.
#' @param pars a [list]
#' @param s the species index
#' @return [numeric]
#' @export
get_MYZinits <- function(pars, s=1) {
  UseMethod("get_MYZinits", pars$MYZpar[[s]])
}

#' @title Set the initial values as a vector
#' @description This method dispatches on the type of `pars$MYZpar`.
#' @param pars a [list]
#' @param y0 a vector of variable values from a simulation
#' @param s the species index
#' @return a [list]
#' @export
update_MYZinits <- function(pars, y0, s) {
  UseMethod("update_MYZinits", pars$MYZpar[[s]])
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_f = function(pars, s=1){
  UseMethod("get_f", pars$MYZpar[[s]]$baseline)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_q = function(pars, s=1){
  UseMethod("get_q", pars$MYZpar[[s]]$baseline)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_g = function(pars, s=1){
  UseMethod("get_g", pars$MYZpar[[s]]$baseline)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_sigma = function(pars, s=1){
  UseMethod("get_sigma", pars$MYZpar[[s]]$baseline)
}

#' @title Compute steady states for **MYZ** Modules
#' @description This method dispatches on the type of `MYZpar`.
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param kappa net infectiousness
#' @param MYZpar a list that defines an adult model
#' @return none
#' @export
xde_steady_state_MYZ = function(Lambda, kappa, MYZpar){
  UseMethod("xde_steady_state_MYZ", MYZpar)
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `MYZpar`.
#' @param Lambda the daily emergence rate of adult mosquitoes
#' @param MYZpar a list that defines an adult model
#' @return none
#' @export
xde_steady_state_M = function(Lambda, MYZpar){
  UseMethod("xde_steady_state_M", MYZpar)
}
