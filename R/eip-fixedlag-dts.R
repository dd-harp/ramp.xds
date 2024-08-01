
#' @title Modify parameters due to exogenous forcing by all kinds of control
#' @description Implements [F_eip] for the fixedlag_dts model (the EIP is constant)
#' @inheritParams F_eip
#' @return the EIP maturation vector G [numeric]
#' @export
F_eip.fixedlag_dts <- function(t, vars, eip_par){
  eip_par$eip
}

#' @title Set up a fixedlag_dts model for the EIP
#' @inheritParams setup_EIP
#' @return [list]
#' @export
setup_EIP.fixedlag_dts <- function(EIPopts, MYZpar){
  setup_eip_fixedlag_dts(EIPopts, MYZpar)
}

#' @title Set up a fixedlag_dts model for the EIP
#' @param EIPopts a [list]
#' @param MYZpar the MYZ parameters
#' @param eip a default value for the eip
#' @param Dday the simulation runtime interval
#' @param MYZday the MYZ component runtime interval
#' @return [list]
#' @export
setup_eip_fixedlag_dts = function(EIPopts, MYZpar, eip=12, Dday=1, MYZday=1){with(EIPopts,{
  EIPmod <- list()
  class(EIPmod) <- 'fixedlag_dts'
  MYZpar$EIPmod <- EIPmod
  MYZpar$eip <- eip*MYZday*Dday
  MYZpar$max_eip <- eip*MYZday*Dday
  MYZpar$G <- rep(0, MYZpar$max_eip)
  MYZpar$G[eip] <- 1
  return(MYZpar)
})}
