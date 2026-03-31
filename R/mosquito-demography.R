#' @title Mosquito Demography 
#' 
#' @description
#' 
#' Mosquito survival and dispersal is described by a *demographic matrix,* denoted \eqn{\Omega.} It is 
#' computed using several parameters:
#' \describe{
#'   \item{`g`}{mortality rate}
#'   \item{`sigma`}{patch emigration}
#'   \item{`mu`}{emigration-related loss}
#'   \item{`K`}{a dispersal matrix}
#' } 
#' 
#' The matrix is computed as:
#'  
#' \deqn{
#'  \Omega = \mbox{diag} \left( g + \sigma \mu \right) - K \cdot \mbox{diag} \left( \sigma \left(1-\mu\right) \right)
#' }
#' 
#' In delay differential equations with a constant EIP (\eqn{\tau}), 
#' survival and dispersal through the EIP is given by: 
#' \deqn{
#'  \Upsilon = e^{-\Omega \tau} 
#' }
#'  
#' @seealso [xds_info_mosquito_dispersal] 
#' 
#' @name xds_info_mosquito_demography
NULL

#' @title Setup the Omega object 
#'
#' @description Set up an object
#' to dispatch updating for the mosquito demographic
#' object, \eqn{\Omega} 
#'
#' @param MY_obj an **`MY`** model object
#'
#' @return a **`MY`** model object
#'
#' @keywords internal
#' @export
setup_Omega_obj = function(MY_obj){
  Omega_obj = list()
  class(Omega_obj) = "setup"
  MY_obj$Omega_obj <- Omega_obj
  MY_obj$Upsilon_obj <- Omega_obj
  return(MY_obj)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param g mosquito death rate, a vector of length `nPatches`
#' @param sigma mosquito emigration rate, a vector of length `nPatches`
#' @param mu emigration loss, a vector of length `nPatches`
#' @param K_matrix a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' @export
compute_Omega_xde <- function(g, sigma, mu, K_matrix){
  if(length(g)==1){
    Omega = matrix(g,1,1)
  } else {
    Omega = diag(g + sigma*mu) - K_matrix %*% diag(sigma*(1-mu)) 
  }
  return(Omega)
}


#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param eip the extrinsic incubation period 
#' @param Omega the demographic matrix 
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' @export
compute_Upsilon_xde <- function(eip, Omega){
  Upsilon = expm::expm(-Omega*eip)
  return(Upsilon)
}

#' @title Make the mosquito demography matrix for spatial RM model in discrete time
#' @param p mosquito daily survival, a vector of length `nPatches`
#' @param ssigma mosquito daily fraction emigrating
#' @param mu emigration survival, a vector of length `nPatches`
#' @param K_matrix a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' @export
compute_Omega_dts <- function(p, ssigma, mu, K_matrix){
  if(length(p)==1){
    Omega = matrix(p,1,1)
  } else {
    Omega = diag(p*(1-ssigma)) + diag(p*mu*ssigma) %*% K_matrix
  }
  return(Omega)
}

#' @title Make the mosquito demography matrix for spatial RM model in continuous time
#' @param eip the extrinsic incubation period 
#' @param Omega the demographic matrix 
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' @export
compute_Upsilon_dts <- function(eip, Omega){
  expm::expm(-Omega*eip)
  return(Omega)
}

#' @title Update Omega for xde 
#' @description Update the demographic matrix
#' for differential equations, if needed 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_xde <- function(xds_obj, s){
  UseMethod("update_Omega_xde", xds_obj$MY_obj[[s]]$Omega_obj)
}

#' @title Update Omega for xde 
#' @description The static case for [update_Omega_xde] 
#' @inheritParams update_Omega_xde
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_xde.static<- function(xds_obj, s){return(xds_obj)}

#' @title Update Omega for xde 
#' @description Compute \eqn{\Omega} and change the class to "setup"
#' @inheritParams update_Omega_xde
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_xde.setup<- function(xds_obj, s=1){
 Omega <- with(xds_obj$MY_obj[[s]], compute_Omega_xde(g, sigma, mu, K_matrix))
 xds_obj$MY_obj[[s]]$Omega <- Omega 
 class(xds_obj$MY_obj[[s]]$Omega_obj) = "static"
 class(xds_obj$MY_obj[[s]]$Upsilon_obj) = "setup"
 return(xds_obj)
}

#' @title Update Omega for xde 
#' @description Compute \eqn{\Omega} 
#' @inheritParams update_Omega_xde
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_xde.dynamic <- function(xds_obj, s){
 Omega <- with(xds_obj$MY_obj[[s]], compute_Omega_xde(g, sigma, mu, K_matrix))
 xds_obj$MY_obj[[s]]$Omega <- Omega 
 browser() 
 return(xds_obj)
}

#' @title Update Omega for dts 
#' @description Update the demographic matrix
#' for differential equations, if needed 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_dts <- function(xds_obj, s){
  UseMethod("update_Omega_dts", xds_obj$MY_obj[[s]]$Omega_obj)
}

#' @title Update Omega for dts 
#' @description The static case for [update_Omega_dts] 
#' @inheritParams update_Omega_dts
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_dts.static<- function(xds_obj, s){return(xds_obj)}

#' @title Update Omega for dts 
#' @description Compute \eqn{\Omega} and change the class to "setup"
#' @inheritParams update_Omega_dts
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_dts.setup<- function(xds_obj, s){
 xds_obj$MY_obj[[s]]$Omega <- with(xds_obj$MY_obj[[s]], compute_Omega_dts(g, sigma, mu, K_matrix))
 class(xds_obj$MY_obj[[s]]$Omega_obj) = "static"
 class(xds_obj$MY_obj[[s]]$Upsilon_obj) = "setup"
 return(xds_obj)
}

#' @title Update Omega for dts 
#' @description Compute \eqn{\Omega} 
#' @inheritParams update_Omega_dts
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega_dts.dynamic <- function(xds_obj, s){
 xds_obj$MY_obj[[s]]$Omega <- with(xds_obj$MY_obj[[s]], compute_Omega_dts(g, sigma, mu, K_matrix))
 return(xds_obj)
}

#' @title Update Upsilon for xde 
#' @description Update the demographic matrix
#' for differential equations, if needed 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_xde <- function(xds_obj, s){
  UseMethod("update_Upsilon_xde", xds_obj$MY_obj[[s]]$Upsilon_obj)
}

#' @title Update Upsilon for xde 
#' @description The static case for [update_Upsilon_xde] 
#' @inheritParams update_Upsilon_xde
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_xde.static<- function(xds_obj, s){return(xds_obj)}

#' @title Update Upsilon for xde 
#' @description Compute \eqn{\Upsilon} and change the class to "setup"
#' @inheritParams update_Upsilon_xde
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_xde.setup<- function(xds_obj, s){
 xds_obj$MY_obj[[s]]$Upsilon <- with(xds_obj$MY_obj[[s]], compute_Upsilon_xde(eip, Omega))
 class(xds_obj$MY_obj[[s]]$Upsilon_obj) = "static"
 return(xds_obj)
}

#' @title Update Upsilon for xde 
#' @description Compute \eqn{\Upsilon} 
#' @inheritParams update_Upsilon_xde
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_xde.dynamic <- function(xds_obj, s){
 xds_obj$MY_obj[[s]]$Upsilon <- with(xds_obj$MY_obj[[s]], compute_Upsilon_xde(eip, Omega))
 return(xds_obj)
}

#' @title Update Upsilon for dts 
#' @description Update the demographic matrix
#' for differential equations, if needed 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_dts <- function(xds_obj, s){
  UseMethod("update_Upsilon_dts", xds_obj$MY_obj[[s]]$Upsilon_obj)
}

#' @title Update Upsilon for dts 
#' @description The static case for [update_Upsilon_dts] 
#' @inheritParams update_Upsilon_dts
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_dts.static<- function(xds_obj, s){return(xds_obj)}

#' @title Update Upsilon for dts 
#' @description Compute \eqn{\Upsilon} and change the class to "setup"
#' @inheritParams update_Upsilon_dts
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_dts.setup<- function(xds_obj, s){
 xds_obj$MY_obj[[s]]$Upsilon <- with(xds_obj$MY_obj[[s]], compute_Upsilon_dts(eip, Omega))
 class(xds_obj$MY_obj[[s]]$Upsilon_obj) = "static"
 return(xds_obj)
}

#' @title Update Upsilon for dts 
#' @description Compute \eqn{\Upsilon} 
#' @inheritParams update_Upsilon_dts
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Upsilon_dts.dynamic <- function(xds_obj, s){
 xds_obj$MY_obj[[s]]$Upsilon <- with(xds_obj$MY_obj[[s]], compute_Upsilon_dts(eip, Omega))
 return(xds_obj)
}
