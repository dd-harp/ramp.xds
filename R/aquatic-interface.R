# generic methods for aquatic component


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
F_alpha <- function(t, y, pars, s) {
  UseMethod("F_alpha", pars$Lpar[[s]])
}

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

#' @title Derivatives for aquatic stage mosquitoes
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param t current simulation time
#' @param y state vector
#' @param pars a [list]
#' @param s the species index
#' @return a [numeric] vector of length `pars$L_ix`
#' @export
DT_Lt <- function(t, y, pars, s) {
  UseMethod("DT_Lt", pars$Lpar[[s]])
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

#' @title A function to set up adult mosquito models
#' @description This method dispatches on `Lname`.
#' @param Lname the class name of the aquatic model
#' @param pars a [list]
#' @param s the species index
#' @param Lopts a [list]
#' @return [list]
#' @export
xde_setup_Lpar = function(Lname, pars, s, Lopts=list()){
  class(Lopts) <- Lname
  UseMethod("xde_setup_Lpar", Lopts)
}

#' @title A function to set up adult mosquito models
#' @description This method dispatches on `Lname`.
#' @param Lname the class name of the aquatic model
#' @param pars a [list]
#' @param s the species index
#' @param Lopts a [list]
#' @return [list]
#' @export
dts_setup_Lpar = function(Lname, pars, s, Lopts=list()){
  class(Lopts) <- Lname
  UseMethod("dts_setup_Lpar", Lopts)
}


#' @title A function to set up adult mosquito models
#' @description This method dispatches on `Lname`.
#' @param pars a [list]
#' @param s the species index
#' @param Lopts a [list]
#' @return [list]
#' @export
setup_Linits = function(pars, s, Lopts=list()){
  UseMethod("setup_Linits", pars$Lpar[[s]])
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

#' @title Return initial values as a vector
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param pars a [list]
#' @param s the species index
#' @return none
#' @export
get_inits_L <- function(pars, s) {
  UseMethod("get_inits_L", pars$Lpar[[s]])
}

#' @title Set the initial values from a vector of model states
#' @description This method dispatches on the type of `pars$Lpar`.
#' @param pars a [list]
#' @param y0 a vector of variable values from a simulation
#' @param s the species index
#' @return none
#' @export
update_inits_L <- function(pars, y0, s) {
  UseMethod("update_inits_L", pars$Lpar[[s]])
}

#' @title Make the habitat membership matrix, calN
#' @param nPatches is the number of patches
#' @param membership is a vector describing the patch where each habitat is found
#' @return a [matrix] of dimensions `nPatches` by `nHabitats`
#' @export
make_calN = function(nPatches, membership){
  nHabitats = length(membership)
  calN = matrix(0, nPatches, nHabitats)
  calN[cbind(membership, 1:nHabitats)]=1
  return(calN)
}


