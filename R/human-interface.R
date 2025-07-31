# generic methods for human component

#' @title Compute Derivatives for the **X** Component 
#' @description Compute and return the derivatives
#' for an **X**-Component module. 
#'
#' @note The function dispatches on `class(Xpar[[i]]).`
#'
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return the derivatives, a [numeric] vector 
#' @export
dXdt <- function(t, y, pars, i) {
  UseMethod("dXdt", pars$Xpar[[i]])
}

#' @title Setup an **X** Module
#' @description Set the parameter values and configure a model 
#' for the **X** Component
#' @param Xname a [character] string
#' @param pars an **`xds`** object
#' @param i the host species index
#' @param Xopts a [list]
#' @return an **`xds`** object
#' @export
setup_Xpar = function(Xname, pars, i, Xopts=list()){
  class(Xname) <- Xname
  UseMethod("setup_Xpar", Xname)
}

#' @title Update X states for a discrete time system
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector
#' @export
Update_Xt <- function(t, y, pars, i) {
  UseMethod("Update_Xt", pars$Xpar[[i]])
}


#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `pars$Xpar[[i]]`
#' @param ar the daily attack rate
#' @param H host density
#' @param Xpar a list that defines a X-class model
#' @return none
#' @export
dts_steady_state_X = function(ar, H, Xpar){
  UseMethod("dts_steady_state_X", Xpar)
}

#' @title Size of effective infectious human population
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @export
F_X <- function(t, y, pars, i) {
  UseMethod("F_X", pars$Xpar[[i]])
}

#' @title Size of human population denominators
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @export
F_H <- function(t, y, pars, i) {
  UseMethod("F_H", pars$Xpar[[i]])
}

#' @title Infection blocking pre-erythrocytic immunity
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param y state vector
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [numeric] vector of length `nStrata`
#' @export
F_b <- function(y, pars, i) {
  UseMethod("F_b", pars$Xpar[[i]])
}


#' @title A function to set up Xpar
#' @description This method dispatches on `pars$Xpar[[i]]`.
#' @param pars an **`xds`** object
#' @param H initial host population density
#' @param i the host species index
#' @param Xopts a [list]
#' @return an **`xds`** object
#' @export
setup_Xinits = function(pars, H, i, Xopts=list()){
  UseMethod("setup_Xinits", pars$Xpar[[i]])
}

#' @title Add indices for human population to parameter list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return an **`xds`** object
#' @export
setup_Xix <- function(pars, i) {
  UseMethod("setup_Xix", pars$Xpar[[i]])
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Xpar[[s]]`.
#' @param y the variables
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
list_Xvars <- function(y, pars, i) {
  UseMethod("list_Xvars", pars$Xpar[[i]])
}

#' @title Return the parameters as a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
get_Xpars <- function(pars, i=1) {
  UseMethod("get_Xpars", pars$Xpar[[i]])
}

#' @title Set new X parameter values
#' @description This method dispatches on the type of `pars$Xpar[[s]]`.
#' @param pars an **`xds`** object
#' @param i the vector species index
#' @param Xopts a named list
#' @return an `xds` object
#' @export
set_Xpars <- function(pars, i=1, Xopts=list()) {
  UseMethod("set_Xpars", pars$Xpar[[i]])
}

#' @title Set new X parameter values
#' @description This method dispatches on the type of `pars$Xpar[[s]]`.
#' @param pars an **`xds`** object
#' @param i the vector species index
#' @param Xopts a named list
#' @return an `xds` object
#' @export
set_Xinits <- function(pars, i=1, Xopts=list()) {
  UseMethod("set_Xinits", pars$Xpar[[i]])
}

#' @title Put Xvars in place of the X variables in y
#' @description This method dispatches on the type of `pars$Xpar[[s]]`.
#' @param Xvars the X variables to insert into y
#' @param y the variables
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return a [list]
#' @export
put_Xvars <- function(Xvars, y, pars, i) {
  UseMethod("put_Xvars", pars$Xpar[[i]])
}

#' @title Parse the output of deSolve and return the variables by name in a list
#' @description This method dispatches on the type of `pars$Xpar[[i]]`. Adds the variables
#' from the X model to a list and returns it
#' @param outputs a [matrix] of outputs from deSolve
#' @param pars an **`xds`** object
#' @param i the host species index
#' @export
parse_Xorbits <- function(outputs, pars, i) {
  UseMethod("parse_Xorbits", pars$Xpar[[i]])
}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return none
#' @export
get_Xinits <- function(pars, i=1) {
  UseMethod("get_Xinits", pars$Xpar[[i]])
}



#' @title Set the initial values from a vector of states
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param pars an **`xds`** object
#' @param y the variables 
#' @param i the host species index
#' @return an **`xds`** object
#' @export
update_Xinits <- function(pars, y, i) {
  UseMethod("update_Xinits", pars$Xpar[[i]])
}

#' @title Compute Net Infectiousness (NI)
#' @description A function to compute NI as an output
#' @param vars a list with the variables attached by name
#' @param Xpar a list defining a model for human
#' @return a [numeric] vector of length `nStrata`
#' @export
F_ni <- function(vars, Xpar) {
  UseMethod("F_ni", Xpar)
}

#' @title Compute the *true* prevalence of infection / parasite rate
#' @description A function that translates the state variables into
#' the "true" *Pf*PR
#' @param vars a list with the variables attached by name
#' @param Xpar a list defining a model for human
#' @return a [numeric] vector of length `nStrata`
#' @export
F_prevalence <- function(vars, Xpar) {
  UseMethod("F_prevalence", Xpar)
}

#' @title Compute the prevalence of infection by light microscopy
#' @description This method dispatches on the type of `pars$Xpar[[i]]`
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_lm <- function(vars, Xpar) {
  UseMethod("F_prevalence", Xpar)
}

#' @title Compute the prevalence of infection by RDT
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by rapid diagnostic test (RDT)
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_rdt <- function(vars, Xpar) {
  UseMethod("F_prevalence", Xpar)
}

#' @title Compute infection prevalence by PCR
#' @description A function that translates the state variables into
#' the predicted *Pf*PR by PCR
#' @note This method dispatches on the type of `pars$Xpar[[i]]`.
#' @inheritParams F_prevalence
#' @return a [numeric] vector of length `nStrata`
#' @export
F_pfpr_by_pcr <- function(vars, Xpar) {
  UseMethod("F_prevalence", Xpar)
}

#' Basic plotting for epidemiological models
#'
#' @param pars an **`xds`** object
#' @param i the host species index
#' @param clrs a vector of colors
#' @param llty an integer (or integers) to set the `lty` for plotting
#' @param add plot axes only if FALSE
#'
#' @export
xds_plot_X = function(pars, i=1, clrs="black", llty=1, add=FALSE){
  UseMethod("xds_plot_X", pars$Xpar[[i]])
}

#' @title Compute the human transmitting capacity
#' @description This method dispatches on the type of `pars$Xpar[[i]]`.
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return none
#' @export
HTC <- function(pars, i) {
  UseMethod("HTC", pars$Xpar[[i]])
}

#' @title Compute Steady States for an **X**-Component Module 
#' @description Compute the steady states as a function of the daily FoI for a
#' static value of human population density
#' @param foi the daily FoI
#' @param H human / host population density
#' @param Xpar a list that defines an **X** model object
#' @return none
#' @export
xde_steady_state_X = function(foi, H, Xpar){
  UseMethod("xde_steady_state_X", Xpar)
}

#' @title Compute Steady States for an **XH**-Component Module 
#' @description Compute the steady states as a function of the daily FoI for
#' a model that has a steady state human / host population density
#' @description This method dispatches on the type of `Xpar`.
#' @param foi the daily FoI
#' @param Xpar a list that defines an \eqn{\cal X} model
#' @return none
#' @export
xde_steady_state_XH = function(foi, Xpar){
  UseMethod("xde_steady_state_X", Xpar)
}
