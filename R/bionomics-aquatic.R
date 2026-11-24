# X

#' @title Set up bionomic port for maturation rate (**L**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline maturation rate,
#' \eqn{psi}, for a static model.
#'
#' @param psi the maturation rate 
#' @param L_obj an **`L`** module object
#'
#' @return an **`L`** module object
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

#' @title Compute the baseline maturation rate (**L**)
#'
#' @description Compute the baseline maturation rate.
#'
#' @param t time
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

#' @title Static model for the maturation rate (**L**)
#' @description Return the baseline maturation rate for a static model.
#' @inheritParams F_psi
#' @return \eqn{psi}, the baseline maturation rate
#' @keywords internal
#' @export
F_psi.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$psi_obj$psi)
}


#' @title Set up bionomic port for density-independent mortality rate (**L**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline density-independent
#' mortality rate, \eqn{phi}, for a static model.
#'
#' @param phi the density-independent mortality rate
#' @param L_obj an **`L`** module object
#'
#' @return an **`L`** module object
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

#' @title Compute the baseline density-independent mortality rate (**L**)
#'
#' @description Compute the baseline density-independent mortality rate.
#'
#' @inheritParams F_psi
#'
#' @return a [numeric] vector of length `nHabitats`
#'
#' @keywords internal
#' @export
F_phi <- function(t, xds_obj, s) {
  UseMethod("F_phi", xds_obj$L_obj[[s]]$phi_obj)
}

#' @title Static model for the density-independent mortality rate (**L**)
#' @description Return the baseline density-independent mortality rate for a static model.
#' @inheritParams F_phi
#' @return \eqn{phi}, the baseline density-independent mortality rate
#' @keywords internal
#' @export
F_phi.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$phi_obj$phi)
}

#' @title Set up bionomic port for delayed maturation parameter (**L**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline delayed maturation
#' parameter in response to mean crowding, \eqn{xi}, for a static model.
#'
#' @param xi the delayed maturation parameter in response to mean crowding
#' @param L_obj an **`L`** module object
#'
#' @return an **`L`** module object
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

#' @title Compute the baseline delayed maturation parameter (**L**)
#'
#' @description Compute the baseline delayed maturation parameter in response to mean crowding.
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

#' @title Static model for the delayed maturation parameter (**L**)
#' @description Return the baseline delayed maturation parameter in response to mean crowding for a static model.
#' @inheritParams F_xi
#' @return \eqn{xi}, the baseline delayed maturation parameter in response to mean crowding
#' @keywords internal
#' @export
F_xi.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$xi_obj$xi)
}

#' @title Set up bionomic port for slope of density-dependent mortality (**L**)
#'
#' @description Set up a [bionomic port][xds_info_port] to compute the baseline slope of
#' density-dependent mortality, \eqn{theta}, for a static model.
#'
#' @param theta the slope of density-dependent mortality
#' @param L_obj an **`L`** module object
#'
#' @return an **`L`** module object
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

#' @title Compute the baseline slope of density-dependent mortality (**L**)
#'
#' @description Compute the baseline slope of density-dependent mortality.
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

#' @title Static model for the slope of density-dependent mortality (**L**)
#' 
#' @description Return the baseline slope of density-dependent mortality for a static model.
#' @inheritParams F_theta
#' @return \eqn{theta}, the baseline slope of density-dependent mortality
#' @keywords internal
#' @export
F_theta.static <- function(t, xds_obj, s){
  return(xds_obj$L_obj[[s]]$theta_obj$theta)
}
