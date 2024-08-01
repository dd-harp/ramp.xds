
#' @title Modify parameters due to exogenous forcing by all kinds of control
#' @description Implements [F_eip] for the static_xde model (the EIP is constant)
#' @inheritParams F_eip
#' @return [numeric]
#' @export
F_eip.static <- function(t, vars, eip_par){
  return(eip_par$eip)
}

#' @title This function computes the negative derivative of the EIP as a function of time
#' @description Implements [d_F_eip_dt] for the static_xde model (deip_dt=0)
#' @inheritParams d_F_eip_dt
#' @return [numeric]
#' @export
d_F_eip_dt.static <- function(t, vars, eip_par){
  return(0)
}

