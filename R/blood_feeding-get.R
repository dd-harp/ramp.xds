
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