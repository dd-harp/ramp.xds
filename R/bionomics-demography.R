#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MY_obj`.
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return an **`xds`** model object
#' @export
setup_Omega <- function(xds_obj, s){
  UseMethod("setup_Omega", xds_obj$xds)
}

#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MY_obj`.
#' @inheritParams setup_Omega
#' @return an **`xds`** model object
#' @export
setup_Omega.xde <- function(xds_obj, s){with(xds_obj$MY_obj[[s]],{
  g = get_g(xds_obj, s)
  sigma = get_sigma(xds_obj, s)
  Omega = make_Omega_xde(g, sigma, mu, K_matrix)
  xds_obj$MY_obj[[s]]$Omega = Omega
  xds_obj$MY_obj[[s]]$Upsilon = expm::expm(-Omega*eip)
  return(xds_obj)
})}

#' @title Make the mosquito demography matrix
#' @note This method dispatches on the type of `xds_obj$MY_obj[[s]]`
#' @inheritParams setup_Omega
#' @return the derivatives a [vector]
#' @export
setup_Omega.dts <- function(xds_obj, s){with(xds_obj$MY_obj[[s]],{
  xds_obj$MY_obj[[s]]$Omega = make_Omega_dts(p, ssigma, mu, K_matrix)
  return(xds_obj)
})}


#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @export
make_Omega <- function(xds_obj, s){
  UseMethod("make_Omega", xds_obj$xds)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @export
make_Omega.xde <- function(xds_obj, s){with(xds_obj$MY_obj[[s]],{
  make_Omega_xde(g, sigma, mu, K_matrix)
})}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param g mosquito death rate, a vector of length `nPatches`
#' @param sigma mosquito emigration rate, a vector of length `nPatches`
#' @param mu emigration loss, a vector of length `nPatches`
#' @param K_matrix a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
make_Omega_xde <- function(g, sigma, mu, K_matrix){
  if(length(g)==1){
    Omega = matrix(g,1,1)
  } else {
    Omega = diag(g) + (diag(1-mu) - K_matrix)%*%diag(sigma)
  }
  return(Omega)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @export
make_Omega.dts <- function(xds_obj, s){with(xds_obj$MY_obj[[s]],{
   make_Omega_dts(p, ssigma, mu, K_matrix)
})}

#' @title Make the mosquito demography matrix for spatial RM model in discrete time
#' @param p mosquito daily survival, a vector of length `nPatches`
#' @param ssigma mosquito daily fraction emigrating
#' @param mu emigration survival, a vector of length `nPatches`
#' @param K_matrix a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @export
make_Omega_dts <- function(p, ssigma, mu, K_matrix){
  if(length(p)==1){
    Omega = matrix(p,1,1)
  } else {
    Omega = diag(p*(1-ssigma)) + diag(p*mu*ssigma) %*% K_matrix
  }
  return(Omega)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @export
make_Upsilon <- function(xds_obj, s){
  UseMethod("make_Upsilon", xds_obj$xds)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @export
make_Upsilon.xde <- function(xds_obj, s){with(xds_obj$MY_obj[[s]],{
  Omega = make_Omega_xde(g, sigma, mu, K_matrix)
  Upsilon = expm(-Omega*eip)
  return(Upsilon)
})}


#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MY_obj`.
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
get_Omega <- function(xds_obj, s=1){
  xds_obj$MY_obj[[s]]$Omega
}

#' @title Make the mosquito demography matrix
#' @description This method dispatches on the type of `MY_obj`.
#' @param xds_obj an **`xds`** model object
#' @param s the species index
#' @return the derivatives a [vector]
#' @export
get_Upsilon <- function(xds_obj, s=1){
  xds_obj$MY_obj[[s]]$Upsilon
}

