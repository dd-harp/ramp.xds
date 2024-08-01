# generic methods for aquatic component


#' @title Derivatives for aquatic stage mosquitoes
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `pars$L_ix`
#' @export
dLdt <- function(t, y, pars, s) {
  UseMethod("dLdt", pars$Lpar[[s]])
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `Lpar`.
#' @param eta the egg-laying rate
#' @param Lpar a list that defines an xde model
#' @return none
#' @export
xde_steady_state_L = function(eta, Lpar){
  UseMethod("xde_steady_state_L", Lpar)
}

#' @title A function to set up adult mosquito models
#' @description This method dispatches on `Lname`.
#' @param Lname the class name of the aquatic model
#' @param pars a [list]
#' @param s the species index
#' @param Lopts a [list]
#' @return [list]
#' @export
make_Lpar = function(Lname, pars, s, Lopts=list()){
  class(Lname) <- Lname
  UseMethod("make_Lpar", Lname)
}


#' @title Derivatives for aquatic stage mosquitoes
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `pars$L_ix`
#' @export
Update_Lt <- function(t, y, pars, s) {
  UseMethod("Update_Lt", pars$Lpar[[s]])
}

#' @title Compute the steady states as a function of the daily EIR
#' @description This method dispatches on the type of `Lpar`.
#' @inheritParams xde_steady_state_L
#' @return none
#' @export
dts_steady_state_L = function(eta, Lpar){
  UseMethod("dts_steady_state_L", Lpar)
}

#' @title Set aquatic bionomic parameter rates relative to baseline
#' @description This method dispatches on the type of `pars$Lpar`. It should
#' compute the values of parameters as a function of exogenous variables
#' or reset the values of the bionomic parameters to baseline values.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [list]
#' @export
LBionomics <- function(t, y, pars, s) {
  UseMethod("LBionomics", pars$Lpar[[s]])
}

#' @title Number of newly emerging adults from each larval habitat
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge <- function(t, y, pars, s) {
  UseMethod("F_emerge", pars$Lpar[[s]])
}

#' @title Return the variables as a list
#' @description This method dispatches on the type of `pars$Lpar[[s]]`.
#' @param y the variables
#' @param pars a [list]
#' @param s the vector species index
#' @return a [list]
#' @export
list_Lvars <- function(y, pars, s) {
  UseMethod("list_Lvars", pars$Lpar[[s]])
}

#' @title Put Lvars in place of the L variables in y
#' @description This method dispatches on the type of `pars$Lpar[[s]]`.
#' @param Lvars the L variables to insert into y
#' @param y the variables
#' @param pars a [list]
#' @param s the vector species index
#' @return a [list]
#' @export
put_Lvars <- function(Lvars, y, pars, s) {
  UseMethod("put_Lvars", pars$Lpar[[s]])
}

#' @title A function to set up adult mosquito models
#' @description This method dispatches on `Lname`.
#' @param pars an `xds` object
#' @param s the species index
#' @param Lopts a [list]
#' @return a modified `xds` object
#' @export
make_Linits = function(pars, s, Lopts=list()){
  UseMethod("make_Linits", pars$Lpar[[s]])
}

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param pars a [list]
#' @param s the species index
#' @return [list]
#' @export
get_Linits <- function(pars, s=1) {
  UseMethod("get_Linits", pars$Lpar[[s]])
}

#' @title Set the initial values from a vector of model states
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param pars a [list]
#' @param y0 a vector of variable values from a simulation
#' @param s the species index
#' @return none
#' @export
update_Linits <- function(pars, y0, s) {
  UseMethod("update_Linits", pars$Lpar[[s]])
}


#' @title Add indices for aquatic stage mosquitoes to parameter list
#' @description This method dispatches on the type of `pars$Lpar`. Adds field `L_ix`
#' to parameter list.
#' @param pars a [list]
#' @param s the species index
#' @return none
#' @export
make_indices_L <- function(pars, s) {
  UseMethod("make_indices_L", pars$Lpar[[s]])
}

#' @title Parse the outputs of solving a dynamical system and return the variables by name in a list
#' @description This method dispatches on the type of `pars$Lpar`. Attaches the
#' state variables for the aquatic ecology model to a list and returns it
#' @param outputs a [matrix] with the solutions to a dynamical system
#' @param pars a [list] that defines the model
#' @param s the species index
#' @export
parse_outputs_L <- function(outputs, pars, s) {
  UseMethod("parse_outputs_L", pars$Lpar[[s]])
}



