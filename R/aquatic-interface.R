# generic methods for aquatic component

#' @title **L** Component Derivatives
#' @description This method computes and returns the derivatives
#' for the **L** Component modules
#' @note Dispatches on the class of `pars$Lpar[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param s the species index
#' @return the derivatives, a [numeric] vector of length \eqn{n_q=}`nHabitats`
#' @export
dLdt <- function(t, y, pars, s) {
  UseMethod("dLdt", pars$Lpar[[s]])
}

#' @title Update States for **L**
#' @description This method updates the state variables
#' for **L** Component modules
#' @note Dispatches on the class of `pars$Lpar[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param s the species index
#' @return the states, a [numeric] vector of length \eqn{n_q=}`nHabitats`
#' @export
Update_Lt <- function(t, y, pars, s) {
  UseMethod("Update_Lt", pars$Lpar[[s]])
}


#' @title Baseline Bionomics for an **L** Component Module
#' @description Handle immature mosquito bionomic parameters as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
LBaseline <- function(t, y, pars, s) {
  UseMethod("LBaseline", pars$Lpar[[s]])
}


#' @title Bionomics for an **L** Component Module
#' @description Handle immature mosquito bionomic parameters as a baseline modified by control
#' @param t current simulation time
#' @param y state vector
#' @param pars an `xds` object
#' @param s the species index
#' @return an `xds` object
#' @export
LBionomics <- function(t, y, pars, s) {
  UseMethod("LBionomics", pars$Lpar[[s]])
}

#' @title Set up `Lpar` for **L** Component modules
#' @description
#' Each instance of `make_Lpar` calls a function (usually `create_Lpar_*`) that
#' creates an object **`Lpar`.**
#' It is attached the **`xds`** object as `pars$Lpar[[s]].` Each instance of `create_Lpar_*`
#' should assign default parameter values. These will be over-written by `Lopts`
#' @note This method assigns `Lname` to class(`Lname`) and dispatches on `Lname`.
#' @param Lname the class name of the aquatic model
#' @param pars an **`xds`** object
#' @param s the species index
#' @param Lopts a named [list] to configure **`Lpar`**
#' @return an **`xds`** object
#' @export
make_Lpar = function(Lname, pars, s, Lopts=list()){
  class(Lname) <- Lname
  UseMethod("make_Lpar", Lname)
}

#' @title Get parameters for the **L** Component module
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a [list]
#' @export
get_Lpars <- function(pars, s=1) {
  UseMethod("get_Lpars", pars$Lpar[[s]])
}

#' @title Set **L** Component Parameters
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param Lopts a named list
#' @return an `xds` object
#' @export
set_Lpars <- function(pars, s=1, Lopts=list()) {
  UseMethod("set_Lpars", pars$Lpar[[s]])
}


#' @title Set **L** Component Initial Values
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param Lopts a named list
#' @return an `xds` object
#' @export
set_Linits <- function(pars, s=1, Lopts=list()) {
  UseMethod("set_Linits", pars$Lpar[[s]])
}

#' @title Emerging adults
#' @description This function computes the rate or number of emerging adults: a
#' rate for differential equations, or a number for difference equations.
#' @note This method dispatches on the class of `pars$Lpar[[s]]`
#' @param t current simulation time
#' @param y state vector
#' @param pars an **`xds`** object
#' @param s the species index
#' @return a [numeric] vector of length `nHabitats`
#' @export
F_emerge <- function(t, y, pars, s) {
  UseMethod("F_emerge", pars$Lpar[[s]])
}

#' @title Return **L** Component Variables as a list
#' @description Extract the variables describing **L** Component
#' states for the \eqn{s^{th}} species and return them as a named list
#' @note This method dispatches on the class of `pars$Lpar[[s]]`.
#' @param y the variables
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a named [list]: the variables of \eqn{\cal L} by name
#' @export
list_Lvars <- function(y, pars, s) {
  UseMethod("list_Lvars", pars$Lpar[[s]])
}

#' @title Put Lvars in place of the L variables in y
#' @description This method dispatches on the class of `pars$Lpar[[s]]`.
#' @param Lvars the L variables to insert into y
#' @param y the variables
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return an **`xds`** object
#' @export
put_Lvars <- function(Lvars, y, pars, s) {
  UseMethod("put_Lvars", pars$Lpar[[s]])
}

#' @title Set up Initial Values of Variables for **L** Component modules
#' @description This method dispatches on `Lname`.
#' @param pars an **`xds`** object
#' @param s the species index
#' @param Lopts a [list]
#' @return an **`xds`** object
#' @export
make_Linits = function(pars, s, Lopts=list()){
  UseMethod("make_Linits", pars$Lpar[[s]])
}

#' @title Get the stored initial values
#' @note This method dispatches on the class of `pars$Lpar`.
#' @param pars an **`xds`** object
#' @param s the species index
#' @return a named [list]
#' @export
get_Linits <- function(pars, s=1) {
  pars$Linits[[s]]
}

#' @title Set Initial Values for the **L** Component Module from a State Vector
#' @param pars an **`xds`** object
#' @param y the state variables
#' @param s the species index
#' @return an `xds` object
#' @export
update_Linits <- function(pars, y, s) {
  UseMethod("update_Linits", pars$Lpar[[s]])
}

#' @title Set the Values of the Indices for **L** Component Modules
#' @param pars an **`xds`** object
#' @param s the species index
#' @return an **`xds`** object
#' @export
make_indices_L <- function(pars, s) {
  UseMethod("make_indices_L", pars$Lpar[[s]])
}

#' @title Parse **L** Component Outputs
#' @description After solving a dynamical system, parse the outputs and
#' return the variables by name in a list.
#' @note This method dispatches on the class of `pars$Lpar[[s]]`
#' @param outputs a [matrix] with the solutions to a dynamical system
#' @param pars an **`xds`** object
#' @param s the species index
#' @return a [list]
#' @export
parse_Lorbits <- function(outputs, pars, s) {
  UseMethod("parse_Lorbits", pars$Lpar[[s]])
}

#' @title Compute steady states for **L** Component Modules
#' @description For differential equation models, compute
#' steady states as a function of daily eggs laid, \eqn{\eta}
#' @note This method dispatches on the class of `Lpar`.
#' @param eta the egg-laying rate
#' @param Lpar a list that defines an xde model
#' @return a named [list]: values of the state variables at the steady state
#' @export
xde_steady_state_L = function(eta, Lpar){
  UseMethod("xde_steady_state_L", Lpar)
}

#' @title Compute steady States for **L** Component Modules
#' @description For discrete time systems, compute
#' steady states as a function of daily eggs laid, \eqn{\eta}
#' @note This method dispatches on the class of `Lpar`.
#' @inheritParams xde_steady_state_L
#' @return a named [list]: values of the state variables at the steady state
#' @export
dts_steady_state_L = function(eta, Lpar){
  UseMethod("dts_steady_state_L", Lpar)
}
