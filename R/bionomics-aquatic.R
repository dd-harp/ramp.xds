
#' @title Setup a Human Fraction Bionomic Object 
#' 
#' @description Set up an object
#' to compute [F_maturation], the maximum 
#' maturation rate \eqn{psi} 
#' 
#' @param psi the human fraction 
#' @param L_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
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

#' @title Compute the blood psieeding rate, psi
#' 
#' @description This method dispatches on the type of `psi_obj`. It should
#' set the values opsi the bionomic parameters to baseline values
#' 
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param s vector species index
#' 
#' @return a [numeric] vector opsi length `nPatches`
#' 
#' @export
F_maturation <- function(t, xds_obj, s) {
  UseMethod("F_maturation", xds_obj$L_obj[[s]]$psi_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_maturation] for a static model
#' @inheritParams F_maturation
#' @return \eqn{psi}, the baseline human fraction 
#' @export
F_maturation.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$psi_obj$psi)
}


#' @title Setup a Human Fraction Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{phi} 
#' 
#' @param phi the human fraction 
#' @param L_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
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
#' @inheritParams F_maturation
#' 
#' @return a [numeric] vector ophi length `nPatches`
#' 
#' @export
F_larval_mort <- function(t, xds_obj, s) {
  UseMethod("F_larval_mort", xds_obj$L_obj[[s]]$phi_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_larval_mort] for a static model
#' @inheritParams F_larval_mort
#' @return \eqn{phi}, the baseline human fraction 
#' @export
F_larval_mort.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$phi_obj$phi)
}

#' @title Setup a Human Fraction Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{xi} 
#' 
#' @param xi the human fraction 
#' @param L_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
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

#' @title Compute the blood xieeding rate, xi
#' 
#' @description This method dispatches on the type of `xi_obj`. It should
#' set the values oxi the bionomic parameters to baseline values
#' 
#' @inheritParams F_maturation
#' 
#' @return a [numeric] vector oxi length `nPatches`
#' 
#' @export
F_dlay_maturation <- function(t, xds_obj, s) {
  UseMethod("F_dlay_maturation", xds_obj$L_obj[[s]]$xi_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_dlay_maturation] for a static model
#' @inheritParams F_dlay_maturation
#' @return \eqn{xi}, the baseline human fraction 
#' @export
F_dlay_maturation.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$xi_obj$xi)
}

#' @title Setup a Human Fraction Bionomic Object 
#' 
#' @description Set up an object
#' to compute the human fraction, \eqn{theta} 
#' 
#' @param theta the human fraction 
#' @param L_obj an **`MY`** model object 
#' 
#' @return a **`MY`** model object
#' 
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

#' @title Compute the blood thetaeeding rate, theta
#' 
#' @description This method dispatches on the type of `theta_obj`. It should
#' set the values otheta the bionomic parameters to baseline values
#' 
#' @inheritParams F_maturation
#' 
#' @return a [numeric] vector otheta length `nPatches`
#' 
#' @export
F_larval_dd_mort <- function(t, xds_obj, s) {
  UseMethod("F_larval_dd_mort", xds_obj$L_obj[[s]]$theta_obj)
}

#' @title Static model for the blood feeding rate
#' @description Implements [F_larval_dd_mort] for a static model
#' @inheritParams F_larval_dd_mort
#' @return \eqn{theta}, the baseline human fraction 
#' @export
F_larval_dd_mort.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$theta_obj$theta)
}
