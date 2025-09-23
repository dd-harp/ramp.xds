
#' @title Get the initial values as a vector
#' @param pars an **`xds`** object
#' @param i the human species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_H = function(pars, i=1){
  y = get_inits(pars, flatten=TRUE)
  F_H(0, y, pars, i)
}

#' @title Get **XH** outputs
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return the orbits for the **XH** component 
#' @export
get_XH = function(pars, i=1){
  got = pars$outputs$orbits$XH[[i]]
  got$time = pars$outputs$orbits$time
  got$eir = pars$outputs$terms$EIR[[i]]
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
#' @param pars an **`xds`** object
#' @param i the host species index
#' @return none
#' @export
get_PR <- function(pars, method = "true", i=1) {
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
get_PR.true <- function(pars, method= "true", i=1) {
  XH <- get_XH(pars, i)
  XH$true_pr
}

#' @title Get the *Pf*PR from a MalariasModel 
#' @description Return the *Pf*PR by PCR
#' @inheritParams get_PR 
#' @return none
#' @export
get_PR.pcr<- function(pars, method = "pcr", i=1) {
  XH <- get_XH(pars, i)
  F_pfpr_by_pcr(XH, pars$Xpar[[i]])
} 

#' @title Get the *Pf*PR from a Malaria Model 
#' @description Return the PR by light microscopy 
#' @inheritParams get_PR 
#' @return none
#' @export
get_PR.lm<- function(pars, method = "lm", i=1) {
  XH <- get_XH(pars, i)
  F_pfpr_by_lm(XH, pars$Xpar[[i]])
}

#' @title Get the *Pf*PR from a Malaria Model 
#' @description Return the PR by RDT
#' @inheritParams get_PR 
#' @return none
#' @export
get_PR.rdt<- function(pars, method = "rdt", i=1) {
  XH <- get_XH(pars, i)
  F_pfpr_by_rdt(XH, pars$Xpar[[i]])
}
