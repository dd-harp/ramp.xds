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

