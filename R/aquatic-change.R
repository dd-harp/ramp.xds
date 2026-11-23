# X
#' @title Change the maturation rate (**L**)
#' @seealso [get_psi]
#'
#' @description Change the stored maturation rate per habitat for a static
#' `basicL` model.
#' @inheritSection basicL Parameters
#' @note Store the supplied values at `xds_obj$L_obj[[s]]$psi`.
#' When `psi_obj` is present, also update `psi_t` and `psi_obj$psi` on that
#' module object. The port class and `es_psi` are unchanged; this function
#' does not evaluate ports or apply effect sizes.
#' Inputs must be numeric, with length 1 or `nHabitats`; scalar values are
#' repeated across habitats. Invalid types or lengths cause an error.
#'
#' @param psi a numeric maturation rate for all habitats or a vector of length `nHabitats`
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @return an **`xds`** model object
#'
#' @export
change_psi = function(psi, xds_obj, s){
  psi = checkIt(psi, xds_obj$nHabitats)
  xds_obj$L_obj[[s]]$psi = psi 
  if (with(xds_obj$L_obj[[s]], exists("psi_obj"))){
    xds_obj$L_obj[[s]]$psi_t = psi
    xds_obj$L_obj[[s]]$psi_obj$psi = psi
  }
  return(xds_obj)
}

#' @title Change the density-independent mortality rate (**L**)
#' @seealso [get_phi]
#'
#' @description Change the stored density-independent mortality rate per
#' habitat for a static `basicL` model.
#' @inheritSection basicL Parameters
#' @note Store the supplied values at `xds_obj$L_obj[[s]]$phi`.
#' When `phi_obj` is present, also update `phi_t` and `phi_obj$phi` on that
#' module object. The port class and `es_phi` are unchanged; this function
#' does not evaluate ports or apply effect sizes.
#' Inputs must be numeric, with length 1 or `nHabitats`; scalar values are
#' repeated across habitats. Invalid types or lengths cause an error.
#'
#' @param phi a numeric density-independent mortality rate for all habitats or a vector of length `nHabitats`
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @return an **`xds`** model object
#'
#' @export
change_phi = function(phi, xds_obj, s){
  phi = checkIt(phi, xds_obj$nHabitats)
  xds_obj$L_obj[[s]]$phi = phi 
  if (with(xds_obj$L_obj[[s]], exists("phi_obj"))){
    xds_obj$L_obj[[s]]$phi_t = phi
    xds_obj$L_obj[[s]]$phi_obj$phi = phi
  }
  return(xds_obj)
}

#' @title Change the delayed maturation parameter (**L**)
#' @seealso [get_xi]
#'
#' @description Change the stored delayed maturation parameter per habitat
#' for a static `basicL` model.
#' @inheritSection basicL Parameters
#' @note Store the supplied values at `xds_obj$L_obj[[s]]$xi`.
#' When `xi_obj` is present, also update `xi_t` and `xi_obj$xi` on that
#' module object. The port class and `es_xi` are unchanged; this function
#' does not evaluate ports or apply effect sizes.
#' Inputs must be numeric, with length 1 or `nHabitats`; scalar values are
#' repeated across habitats. Invalid types or lengths cause an error.
#'
#' @param xi a numeric delayed maturation parameter for all habitats or a vector of length `nHabitats`
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @return an **`xds`** model object
#'
#' @export
change_xi = function(xi, xds_obj, s){
  xi = checkIt(xi, xds_obj$nHabitats)
  xds_obj$L_obj[[s]]$xi = xi 
  if (with(xds_obj$L_obj[[s]], exists("xi_obj"))){
    xds_obj$L_obj[[s]]$xi_t = xi
    xds_obj$L_obj[[s]]$xi_obj$xi = xi
  }
  return(xds_obj)
}

#' @title Change the slope of density-dependent mortality (**L**)
#' @seealso [get_theta]
#'
#' @description Change the stored slope of density-dependent mortality per
#' habitat for a static `basicL` model.
#' @inheritSection basicL Parameters
#' @note Store the supplied values at `xds_obj$L_obj[[s]]$theta`.
#' When `theta_obj` is present, also update `theta_t` and `theta_obj$theta`
#' on that module object. The port class and `es_theta` are unchanged;
#' this function does not evaluate ports or apply effect sizes.
#' Inputs must be numeric, with length 1 or `nHabitats`; scalar values are
#' repeated across habitats. Invalid types or lengths cause an error.
#'
#' @param theta a numeric slope of density-dependent mortality for all habitats or a vector of length `nHabitats`
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @return an **`xds`** model object
#'
#' @export
change_theta = function(theta, xds_obj, s){
  theta = checkIt(theta, xds_obj$nHabitats)
  xds_obj$L_obj[[s]]$theta = theta 
  if (with(xds_obj$L_obj[[s]], exists("theta_obj"))){
    xds_obj$L_obj[[s]]$theta_t = theta
    xds_obj$L_obj[[s]]$theta_obj$theta = theta
  }
  return(xds_obj)
}