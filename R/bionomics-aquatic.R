
#' @title Setup psi object (maturation rate) 
#'
#' @description Set up an object
#' to compute the maturation  
#' rate \eqn{psi}
#'
#' @param psi the maturation rate 
#' @param L_obj an **`L`** model object
#'
#' @return a **`L`** model object
#' @keywords internal
#'
#' @export
setup_psi_obj = function(psi, L_obj){
  L_obj$psi = psi
  L_obj$psi_t = psi
  L_obj$es_psi = 1
  L_obj$psi_obj <- list()
  class(L_obj$psi_obj) <- "static"
  L_obj$psi_obj$psi <- psi
  return(L_obj)
}

#' @title Setup for  
#'
#' @description Set up an object
#' to compute 
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#'
#' @return a [numeric] vector of length `nHabitats`
#'
#' @keywords internal
#' @export
F_psi <- function(t, xds_obj, s) {
  UseMethod("F_psi", xds_obj$L_obj[[s]]$psi_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_psi] for a static model
#' @inheritParams F_psi
#' @return \eqn{psi}, the baseline human fraction
#' @keywords internal
#' @export
F_psi.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$psi_obj$psi)
}


#' @title Setup phi object (density independent mortality) 
#'
#' @description Set up an object
#' to compute the density independent death rate, \eqn{phi}
#'
#' @param phi the human fraction
#' @param L_obj an **`L`** model object
#'
#' @return a **`L`** model object
#' @keywords internal
#'
#' @export
setup_phi_obj = function(phi, L_obj){
  L_obj$phi = phi
  L_obj$phi_t = phi
  L_obj$es_phi = 1
  L_obj$phi_obj <- list()
  class(L_obj$phi_obj) <- "static"
  L_obj$phi_obj$phi <- phi
  return(L_obj)
}

#' @title Compute the blood phieeding rate, phi
#'
#' @description This method dispatches on the type of `phi_obj`. It should
#' set the values ophi the bionomic parameters to baseline values
#'
#' @inheritParams F_psi
#'
#' @return a [numeric] vector ophi length `nHabitats`
#'
#' @keywords internal
#' @export
F_phi <- function(t, xds_obj, s) {
  UseMethod("F_phi", xds_obj$L_obj[[s]]$phi_obj)
}

#' @title Compute phi, density independent mortality rate 
#' @description Implements [F_phi] for a static model
#' @inheritParams F_phi
#' @return \eqn{phi}, the baseline human fraction
#' @keywords internal
#' @export
F_phi.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$phi_obj$phi)
}

#' @title Setup xi object (maturation delay) 
#'
#' @description Set up an object
#' to compute the delayed maturation parameter, \eqn{xi}
#'
#' @param xi the human fraction
#' @param L_obj an **`L`** model object
#'
#' @return a **`L`** model object
#' @keywords internal
#' @export
setup_xi_obj = function(xi, L_obj){
  L_obj$xi = xi
  L_obj$xi_t = xi
  L_obj$es_xi = 1
  L_obj$xi_obj <- list()
  class(L_obj$xi_obj) <- "static"
  L_obj$xi_obj$xi <- xi
  return(L_obj)
}

#' @title Compute xi, the maturation delay parameter 
#'
#' @description This method dispatches on the type of `xi_obj`. It should
#' set the values oxi the bionomic parameters to baseline values
#'
#' @inheritParams F_psi
#'
#' @return a [numeric] vector of length `nHabitats`
#'
#' @keywords internal
#' @export
F_xi <- function(t, xds_obj, s) {
  UseMethod("F_xi", xds_obj$L_obj[[s]]$xi_obj)
}

#' @title Static model for the maturation delay parameter 
#' @description Implements [F_xi] for a static model
#' @inheritParams F_xi
#' @return \eqn{xi}, the baseline human fraction
#' @keywords internal
#' @export
F_xi.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$xi_obj$xi)
}

#' @title Setup theta object (density dependent larval mortality) 
#'
#' @description Set up an object
#' to compute excess mortality as a function of 
#' to mean crowding, \eqn{theta}
#'
#' @param theta the human fraction
#' @param L_obj an **`L`** model object
#'
#' @return a **`L`** model object
#' @keywords internal
#' @export
setup_theta_obj = function(theta, L_obj){
  L_obj$theta = theta
  L_obj$theta_t = theta
  L_obj$es_theta = 1
  L_obj$theta_obj <- list()
  class(L_obj$theta_obj) <- "static"
  L_obj$theta_obj$theta <- theta
  return(L_obj)
}

#' @title Compute theta (density dependent mortality) 
#'
#' @description This method dispatches on the type of `theta_obj`. It should
#' set the values otheta the bionomic parameters to baseline values
#'
#' @inheritParams F_psi
#'
#' @return a [numeric] vector of length `nHabitats`
#'
#' @keywords internal
#' @export
F_theta <- function(t, xds_obj, s) {
  UseMethod("F_theta", xds_obj$L_obj[[s]]$theta_obj)
}

#' @title Compute theta (density dependent mortality) 
#' 
#' @description Implements [F_theta] for a static model
#' @inheritParams F_theta
#' @return \eqn{theta}, slope of density-dependent mortality  
#' @keywords internal
#' @export
F_theta.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$theta_obj$theta)
}
