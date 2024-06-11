#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param t current simulation time
#' @param pars a [list]
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
make_Omega <- function(t, pars, s) {
  UseMethod("make_Omega", pars$MYZpar[[s]]$Omega_par)
}

#' @title Handle the mosquito demography matrix for static models
#' @inheritParams make_Omega
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
make_Omega.static <- function(t, pars, s){
  return(pars$MYZpar[[s]]$Omega)
}

#' @title Make Upsilon for delay differential equations
#' @description This method dispatches on the type of `pars$MYZpar[[s]]`.
#' @param t current simulation time
#' @param pars a [list]
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
make_Upsilon <- function(t, pars, s) {
  UseMethod("make_Upsilon", pars$MYZpar[[s]]$Omega_par)
}

#' @title Handle the mosquito demography matrix for static models
#' @inheritParams make_Upsilon
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
make_Upsilon.static <- function(t, pars, s){
  return(pars$MYZpar[[s]]$Upsilon)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param g mosquito death rate, a vector of length `nPatches`
#' @param sigma mosquito emigration rate, a vector of length `nPatches`
#' @param mu emigration loss, a vector of length `nPatches`
#' @param calK a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
make_Omega_xde <- function(g, sigma, mu, calK){
  if(length(g)==1){
    Omega = matrix(g,1,1)
  } else {
    Omega = diag(g) + (diag(1-mu) - calK)%*%diag(sigma)
  }
  return(Omega)
}

#' @title Make the mosquito demography matrix for spatial RM model in discrete time
#' @param p mosquito daily survival, a vector of length `nPatches`
#' @param sigma mosquito daily emigration fraction, a vector of length `nPatches`
#' @param mu emigration survival, a vector of length `nPatches`
#' @param calK a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
make_Omega_dts <- function(p, sigma, mu, calK){
  if(length(p)==1){
    Omega = matrix(p,1,1)
  } else {
    Omega = diag(p*(1-sigma)) + diag(p*mu*sigma) %*% calK
  }
  return(Omega)
}

