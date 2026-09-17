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

#' @title F_Omega for xde
#' 
#' @description
#' Computes the mosquito demographic matrix:
#' \deqn{
#'  \Omega = \mbox{diag} \left( g + \sigma \mu \right) - K \cdot \mbox{diag} \left( \sigma \left(1-\mu\right) \right)
#' }
#' 
#' @param g mosquito death rate, a vector of length `nPatches`
#' @param sigma mosquito emigration rate, a vector of length `nPatches`
#' @param mu emigration loss, a vector of length `nPatches`
#' @param K_matrix a [matrix] of dimensions `nPatches` by `nPatches`
#' 
#' @seealso [xds_info_mosquito_demography]
#' 
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' 
#' @keywords internal
#' @export
F_Omega_xde <- function(g, sigma, mu, K_matrix){
  if(length(g)==1){
    Omega = matrix(g,1,1)
  } else {
    Omega = diag(g + sigma*mu) - K_matrix %*% diag(sigma*(1-mu)) 
  }
  return(Omega)
}

#' @title F_Upsilon for dts
#' @param eip the extrinsic incubation period 
#' @param Omega the demographic matrix 
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' @export
F_Upsilon_xde <- function(eip, Omega){
  Upsilon = expm::expm(-Omega*eip)
  return(Upsilon)
}

#' @title F_Omega for dts
#' @param p mosquito daily survival, a vector of length `nPatches`
#' @param ssigma mosquito daily fraction emigrating
#' @param mu emigration survival, a vector of length `nPatches`
#' @param K_matrix a [matrix] of dimensions `nPatches` by `nPatches`
#' @return a [matrix] of dimensions `nPatches` by `nPatches`
#' @keywords internal
#' @export
F_Omega_dts <- function(p, ssigma, mu, K_matrix){
  if(length(p)==1){
    Omega = matrix(p,1,1)
  } else {
    Omega = diag(p*(1-ssigma)) + diag(p*mu*ssigma) %*% K_matrix
  }
  return(Omega)
}

#' @title Setup the Omega obj
#' 
#' @description The Omega object handles
#' dispatching for `change_Omega` and `update_Omega`
#' 
#' Options for `change_Omega` are:
#' + `xde` --- for most differential equation systems
#' + `dts` --- for most discrete time systems
#' 
#' Options for behavioral state models are in `ramp.library`
#'  
#' Options for `update_Omega` are:
#' + `static` --- for autonomous systems
#' + `dynamic` --- for non-autonomous systems
#'  
#' @param ch is `xde`, `dts`
#' @param up is `static` or `dynamic`
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return an **xds** model object 
#' @keywords internal
#' @export
setup_Omega_obj = function(ch, up, xds_obj, s=1){
  stopifnot(ch %in% c("xde", "dts"))
  stopifnot(up %in% c("static", "dynamic"))
  O_obj <- list(ch, up)
  O_obj$change = ch
  O_obj$update = up
  class(O_obj) <- c(ch, up)
  xds_obj$MY_obj[[s]]$Omega_obj <- O_obj
  return(xds_obj)
}

#' @title Change Omega 
#' 
#' @description Update a static demographic matrix
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
change_Omega = function(xds_obj, s){
  UseMethod("change_Omega", xds_obj$MY_obj[[s]]$Omega_obj)
}



#' @title Change Omega
#'
#' @description Update the demographic matrix for
#' differential equations
#'
#' @inheritParams change_Omega
#'
#' @return an **xds** model object
#' @keywords internal
#' @export
change_Omega.xde = function(xds_obj, s){
  with(xds_obj$MY_obj[[s]],{
    xds_obj$MY_obj[[s]]$Omega <- F_Omega_xde(g, sigma, mu, K_matrix)
    xds_obj <- change_Upsilon(xds_obj, s)
    return(xds_obj)
  })}

#' @title Change Omega 
#' 
#' @description Update the demographic matrix for 
#' discrete time systems
#' 
#' @inheritParams change_Omega
#' 
#' @return an **xds** model object 
#' @keywords internal
#' @export
change_Omega.dts = function(xds_obj, s){
  with(xds_obj$MY_obj[[s]],{
    xds_obj$MY_obj[[s]]$Omega <- F_Omega_dts(p, sigma, mu, K_matrix)
    return(xds_obj)
})}

#' @title Change Omega 
#' 
#' @description Update a static demographic matrix
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega = function(xds_obj, s){
  UseMethod("update_Omega", xds_obj$MY_obj[[s]]$Omega_obj)
}

#' @title Change Omega 
#' 
#' @description Update a static demographic matrix
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega.static = function(xds_obj, s){
  return(xds_obj)
}

#' @title Change Omega 
#' 
#' @description Update a static demographic matrix
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @return an **xds** model object 
#' @keywords internal
#' @export
update_Omega.dynamic = function(xds_obj, s){
  change_Omega(xds_obj, s)
}

#' @title Set up the Upsilon object
#' 
#' @description Set to `dde` if **MY** is a 
#' delay differential equation 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' @param dede is it a delay differential equation that needs \eqn{\Upsilon}
#' 
#' @return an **xds** model object 
#' @keywords internal
#' @export
setup_Upsilon_obj = function(xds_obj, s=1, dede=FALSE){
  U_obj = list()
  class(U_obj) = ifelse(dede, "dde", "na")
  xds_obj$MY_obj[[s]]$Upsilon_obj <- U_obj
  return(xds_obj) 
}

#' @title Change Upsilon
#' 
#' @description Update \eqn{\Upsilon}, the matrix describing
#' mosquito survival and dispersal through an EIP in delay equations
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return an **xds** model object 
#' @keywords internal
#' @export
change_Upsilon = function(xds_obj, s=1){
  UseMethod("change_Upsilon", xds_obj$MY_obj[[s]]$Upsilon_obj)
}

#' @title Change Upsilon
#' 
#' @description Update \eqn{\Upsilon}, the matrix describing
#' mosquito survival and dispersal through an EIP in delay equations
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return an **xds** model object 
#' @keywords internal
#' @export
change_Upsilon.na = function(xds_obj, s=1){
  return(xds_obj)
}

#' @title Change Upsilon
#' 
#' @description Update \eqn{\Upsilon}, the matrix describing
#' mosquito survival and dispersal through an EIP in delay equations
#' 
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return an **xds** model object 
#' @keywords internal
#' @export
change_Upsilon.dde = function(xds_obj, s=1){
  with(xds_obj$MY_obj[[s]],{
    xds_obj$MY_obj[[s]]$Upsilon <- F_Upsilon_xde(eip, Omega)
    return(xds_obj)
  })}


