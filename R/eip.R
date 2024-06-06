# generic methods to compute the extrinsic incubation period (EIP)

#' @title Compute the EIP
#' @description This method dispatches on the type of `EIPmod`.
#' @param t current simulation time
#' @param MYZpar a [list]
#' @return [numeric] the eip
#' @export
EIP <- function(t, MYZpar) {
  UseMethod("EIP", MYZpar$EIPmod)
}

#' @title Compute the derivative of the EIP as a function of time
#' @description This method dispatches on the type of `EIPmod`.
#' @param t current simulation time
#' @param EIPmod a [list]
#' @return [numeric]
#' @export
dEIPdt <- function(t, EIPmod) {
  UseMethod("dEIPdt", EIPmod)
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

