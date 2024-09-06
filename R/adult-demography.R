#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MYZpar`.
#' @param pars a [list]
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
make_Omega <- function(pars, s){
  UseMethod("make_Omega", pars$xds)
}

#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MYZpar`.
#' @inheritParams make_Omega
#' @return the derivatives a [vector]
#' @export
make_Omega.xde <- function(pars, s){with(pars$MYZpar[[s]],{
  g = get_g(pars, s)
  sigma = get_sigma(pars, s)
  Omega = compute_Omega_xde(g, sigma, mu, calK)
  pars$MYZpar[[s]]$Omega = Omega
  pars$MYZpar[[s]]$Upsilon = expm::expm(-Omega*eip)
  return(pars)
})}

#' @title Make the mosquito demography matrix
#' @note This method dispatches on the type of `pars$MYZpar[[s]]`
#' @inheritParams make_Omega
#' @return the derivatives a [vector]
#' @export
make_Omega.dts <- function(pars, s){with(pars$MYZpar[[s]],{
  pars$MYZpar[[s]]$Omega = compute_Omega_dts(p, ssigma, mu, calK)
  return(pars)
})}


#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param pars a [list]
#' @param s the species index
#' @export
compute_Omega <- function(pars, s){
  UseMethod("compute_Omega", pars$xds)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param pars a [list]
#' @param s the species index
#' @export
compute_Omega.xde <- function(pars, s){with(pars$MYZpar[[s]],{
  compute_Omega_xde(g, sigma, mu, calK)
})}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param g mosquito death rate, a vector of length `nPatches`
#' @param sigma mosquito emigration rate, a vector of length `nPatches`
#' @param mu emigration loss, a vector of length `nPatches`
#' @param calK a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
compute_Omega_xde <- function(g, sigma, mu, calK){
  if(length(g)==1){
    Omega = matrix(g,1,1)
  } else {
    Omega = diag(g) + (diag(1-mu) - calK)%*%diag(sigma)
  }
  return(Omega)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param pars a [list]
#' @param s the species index
#' @export
compute_Omega.dts <- function(pars, s){with(pars$MYZpar[[s]],{
   compute_Omega_dts(p, ssigma, mu, calK)
})}

#' @title Make the mosquito demography matrix for spatial RM model in discrete time
#' @param p mosquito daily survival, a vector of length `nPatches`
#' @param ssigma mosquito daily fraction emigrating
#' @param mu emigration survival, a vector of length `nPatches`
#' @param calK a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
compute_Omega_dts <- function(p, ssigma, mu, calK){
  if(length(p)==1){
    Omega = matrix(p,1,1)
  } else {
    Omega = diag(p*(1-ssigma)) + diag(p*mu*ssigma) %*% calK
  }
  return(Omega)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param pars a [list]
#' @param s the species index
#' @export
compute_Upsilon <- function(pars, s){
  UseMethod("compute_Upsilon", pars$xds)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param pars a [list]
#' @param s the species index
#' @export
compute_Upsilon.xde <- function(pars, s){with(pars$MYZpar[[s]],{
  Omega = compute_Omega_xde(g, sigma, mu, calK)
  Upsilon = expm(-Omega*eip)
  return(Upsilon)
})}
