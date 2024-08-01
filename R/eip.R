# generic methods to compute the extrinsic incubation period (EIP)

#' @title Compute the EIP
#' @description This method ...
#' @param t current simulation time
#' @param vars exogenous variables
#' @param eip_par a [list]
#' @return a [numeric] vector of length `nPatches`
#' @export
F_eip <- function(t, vars, eip_par) {
  UseMethod("F_eip", eip_par)
}

#' @title Compute the derivative of the EIP as a function of time
#' @description This method ...
#' @param t current simulation time
#' @param vars exogenous variables
#' @param eip_par a [list]
#' @return [numeric]
#' @export
d_F_eip_dt <- function(t, vars, eip_par) {
  UseMethod("d_F_eip_dt", eip_par)
}

#' @title Set up the fixed model for control forcing (do nothing)
#' @param EIPopts a list with options to setup EIPmod; must include EIPmod$EIPname
#' @param MYZpar the MYZ parameters
#' @return [list] MYZpar with the EIPmod attached
#' @export
setup_EIP <- function(EIPopts, MYZpar) {
  class(EIPopts) <- EIPopts$EIPname
  UseMethod("setup_EIP", EIPopts)
}

