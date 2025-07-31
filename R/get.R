#' @title Get the initial values as a vector
#' @param pars an **`xds`** object
#' @param i the human species index
#' @return y a [numeric] vector assigned the class "dynamic"
#' @export
get_H = function(pars, i=1){
  y = get_inits(pars, flatten=TRUE)
  F_H(0, y, pars, i)
}


#' @title Get the last state
#' @param pars an `xds` object
#' @return a [numeric] vector
#' @export
get_last <- function(pars){
  pars$outputs$last_y
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm a vector of times
#' @param de_vars a matrix with the values of the variables
#' @param pars an **`xds`** object
#' @return [list]
#' @export
get_terms <- function(tm, de_vars, pars) {
  pars <- reset_state_i(1, tm, de_vars, pars)
  terms = list()
  terms$EIR = as.vector(pars$EIR[[1]])
  terms$kappa = as.vector(pars$kappa[[1]])
  for(i in 2:length(tm)){
    pars <- reset_state_i(i, tm, de_vars, pars)
    terms$EIR <- rbind(terms$EIR, as.vector(pars$EIR[[1]]))
    terms$kappa <- rbind(terms$kappa, as.vector(pars$kappa[[1]]))
  }
  return(terms)
}

#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param i the host species index
#' @export
get_EIR = function(pars, i){
  pars$outputs$terms$EIR
}

#' @title Get **MYZ** outputs
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @export
get_MYZ = function(pars, s=1){
  got = pars$outputs$orbits$MYZ[[s]]
  got$time = pars$outputs$orbits$time
  got$kappa = pars$outputs$terms$kappa[[s]]
  return(got)
}

#' @title Get **XH** outputs
#' @param pars an **`xds`** object
#' @param i the host species index
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
#' @return none
#' @export
get_PR.true <- function(pars, method= "true", i=1) {
  XH <- get_XH(pars, i)$true_pr
}

#' @title Get the *Pf*PR from a Malaria Model 
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