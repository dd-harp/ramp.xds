
#' @title Get the feeding rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a vector
#' @seealso [get_bionomics]
#' @export
get_ft = function(pars, s=1){
  return(pars$outputs$bionomics[[s]]$f)
}

#' @title Get the human fraction
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a vector
#' @export
get_qt = function(pars, s=1){
  return(pars$outputs$bionomics[[s]]$q)
}

#' @title Get the mosquito mortality rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a vector
#' @export
get_gt = function(pars, s=1){
  return(pars$outputs$bionomics[[s]]$g)
}

#' @title Get the mosquito emigration rate
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @return a vector
#' @export
get_sigmat = function(pars, s=1){
  return(pars$outputs$bionomics[[s]]$sigma)
}


#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MYZpar`.
#' @param pars a [list]
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
get_Omega <- function(pars, s){
  pars$MYZpar[[s]]$Omega
}

#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MYZpar`.
#' @param pars a [list]
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
get_Upsilon <- function(pars, s){
  pars$MYZpar[[s]]$Upsilon
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm the time
#' @param de_vars the output of [deSolve]
#' @param pars an `xds` object
#' @return a named [list]
#' @export
get_bionomics <- function(tm, de_vars, pars) {
  MYZ <- list()
  MYZ[[1]] <- get_bionomics_s(tm, de_vars, pars, 1)
  s = length(pars$MYZpar)
  if(s >1)
    for(ix in 2:s)
      MYZ[[ix]] <- get_bionomics_s(tm, de_vars, pars, ix)
  return(MYZ)
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm the time
#' @param de_vars the output of [deSolve]
#' @param pars an `xds` object
#' @param s a vector species index
#' @return [list]
#' @export
get_bionomics_s <- function(tm, de_vars, pars, s) {

  pars <- reset_state_i(1, tm, de_vars, pars)
  f     <- get_f(pars, s)
  q     <- get_q(pars, s)
  g     <- get_g(pars, s)
  sigma <- get_sigma(pars, s)
  Omega <- compute_Omega(pars, s)
  Upsilon <- compute_Upsilon(pars, s)

  for(i in 2:length(tm)){
    pars <- reset_state_i(i, tm, de_vars, pars)
    f     <- rbind(f, get_f(pars, s))
    q     <- rbind(q, get_q(pars, s))
    g     <- rbind(g, get_g(pars, s))
    sigma <- rbind(sigma, get_sigma(pars, s))
  }
  return(list(f=f, q=q, g=g, sigma=sigma))
}

#' @title Compute dynamical terms
#' @description Using the output of deSolve
#' compute the dynamical terms for the output of `xde_solve.ode` or `xde_solve.dde`
#' @param tm the time
#' @param de_vars the output of [deSolve]
#' @param pars an `xds` object
#' @param s a vector species index
#' @return [list]
#' @export
get_bionomics_s_t <- function(tm, de_vars, pars, s) {
  pars <- reset_state(tm, de_vars, pars)
  f     <- get_f(pars, s)
  q     <- get_q(pars, s)
  g     <- get_g(pars, s)
  sigma <- get_sigma(pars, s)
  Omega <- compute_Omega(pars, s)
  Upsilon <- compute_Upsilon(pars, s)
  return(list(f=f, q=q, g=g, sigma=sigma, Omega=Omega, Upsilon=Upsilon))
}
