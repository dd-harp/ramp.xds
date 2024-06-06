
#' @title Modify parameters due to exogenous forcing by all kinds of control
#' @description Implements [EIP] for the static_xde model (the EIP is constant)
#' @inheritParams EIP
#' @return [numeric]
#' @export
EIP.static_xde <- function(t, MYZpar){
  return(MYZpar)
}

#' @title This function computes the negative derivative of the EIP as a function of time
#' @description Implements [dEIPdt] for the static_xde model (the dEIPdt=0)
#' @inheritParams dEIPdt
#' @return [numeric]
#' @export
dEIPdt.static_xde <- function(t, EIPmod){
  return(0)
}


#' @title Set up a static_xde model for the EIP
#' @inheritParams setup_EIP
#' @return [list]
#' @export
setup_EIP.static_xde<- function(EIPopts, MYZpar){
  setup_eip_static_xde(EIPopts, MYZpar)
}

#' @title Set up a static_xde model for the EIP
#' @param EIPopts a [list]
#' @param MYZpar a [list]
#' @param eip the extrinsic incubation period (in days)
#' @return [list]
#' @export
setup_eip_static_xde = function(EIPopts, MYZpar, eip=12){with(EIPopts,{
  EIPmod <- list()
  class(EIPmod) <- 'static_xde'
  MYZpar$EIPmod = EIPmod
  MYZpar$eip = eip
  return(MYZpar)
})}
