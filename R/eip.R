# generic methods to compute the extrinsic incubation period (EIP)

#' @title Setup an EIP Bionomic Object 
#' 
#' @description Set up an object
#' to compute the EIP 
#' 
#' @param eip the mosquito patch emigration rate
#' @param MY_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
#' @export
setup_eip_obj = function(eip, MY_obj){
  MY_obj$eip = eip
  MY_obj$eip_obj <- list()  
  class(MY_obj$eip_obj) <- "static" 
  MY_obj$eip_obj$eip <- eip 
  MY_obj$eip_obj$dF_eip <- F_zero 
  return(MY_obj)
}

#' @title Compute the EIP
#' 
#' @description This method dispatches on the type of `eip_obj`. It 
#' sets the values the EIP
#' 
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return a [numeric] vector oeip length `nPatches`
#' 
#' @export
F_eip <- function(t, xds_obj, s){
  UseMethod("F_eip", xds_obj$MY_obj[[s]]$eip_obj)
}

#' @title Static model patch emigration 
#' 
#' @description Implements [F_eip] for a static model
#' 
#' @inheritParams F_eip
#' 
#' @return \eqn{eip}, the patch emigration rate 
#' @export
F_eip.static <- function(t, xds_obj, s){
  return(xds_obj$MY_obj[[s]]$eip_obj$eip)
}

