
#' @title Get the initial values as a vector
#' 
#' @param xds_obj an **`xds`** object
#' @param i the human species index
#' 
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_H = function(xds_obj, i=1){
  y = get_inits(xds_obj, flatten=TRUE)
  F_H(0, y, xds_obj, i)
}

#' @title Get **XH** outputs
#' 
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' 
#' @return the orbits for the **XH** component 
#' 
#' @export
get_XH_out = function(xds_obj, i=1){
  
  got = xds_obj$outputs$orbits$XH[[i]]
  got$time = xds_obj$outputs$time
  
  return(got)
}

#' @title Get the *Pf*PR from a Malaria Model 
#' @description This method assigns `class(method)=method` and then 
#' dispatches on "type." Options are: 
#' + `true` for the true *Pf*PR  
#' + `lm` for the *Pf*PR by light microscopy
#' + `rdt` for the *Pf*PR by RDT 
#' + `pcr` for the *Pf*PR by PCR 
#' @param method the method used for computing *Pf*PR 
#' @param xds_obj an **`xds`** object
#' @param i the host species index
#' @return none
#' @export
get_PR <- function(xds_obj, i=1, method="true") {
  class(method) = method
  UseMethod("get_PR", method)
}

#' @title Get the *Pf*PR from a Malaria Model 
#' @description  Return the true *Pf*PR
#' 
#' @inheritParams get_PR 
#' 
#' @return the true PR 
#' @export
get_PR.true <- function(xds_obj, i=1, method="true") {
  get_XH_out(xds_obj, i)$true_pr
}

#' @title Get the *Pf*PR from a MalariasModel 
#' @description Return the *Pf*PR by PCR
#' @inheritParams get_PR 
#' @return none
#' @export
get_PR.pcr<- function(xds_obj, i=1, method="pcr") {
  XH <- get_XH_out(xds_obj, i)
  F_pfpr_by_pcr(XH, xds_obj$XH_obj[[i]])
} 

#' @title Get the *Pf*PR from a Malaria Model 
#' @description Return the PR by light microscopy 
#' @inheritParams get_PR 
#' @return none
#' @export
get_PR.lm<- function(xds_obj, i=1, method = "lm") {
  XH <- get_XH_out(xds_obj, i)
  F_pfpr_by_lm(XH, xds_obj$XH_obj[[i]])
}

#' @title Get the *Pf*PR from a Malaria Model 
#' @description Return the PR by RDT
#' @inheritParams get_PR 
#' @return none
#' @export
get_PR.rdt<- function(xds_obj, i=1, method = "rdt") {
  XH <- get_XH_out(xds_obj, i)
  F_pfpr_by_rdt(XH, xds_obj$XH_obj[[i]])
}
