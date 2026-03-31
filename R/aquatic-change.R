#' @title Change psi  
#'
#' @description Change the maturation 
#' rate for basicL for a static model 
#'
#' @param psi the maturation rate 
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
    xds_obj$L_obj[[s]]$f_obj$psi = psi 
  }
  return(xds_obj)
}

#' @title Change phi  
#'
#' @description Change the density independent 
#' mortality rate for basicL for a static model 
#'
#' @param phi the density independent mortality rate 
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
    xds_obj$L_obj[[s]]$f_obj$phi = phi 
  }
  return(xds_obj)
}

#' @title Change xi, the maturation delay parameter
#'
#' @description Change the maturation delay 
#' parameter for basicL for a static model 
#'
#' @param xi the density independent mortality rate 
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
    xds_obj$L_obj[[s]]$f_obj$xi = xi 
  }
  return(xds_obj)
}

#' @title Change theta, density dependent mortality 
#'
#' @description Change the density dependent mortality 
#' parameter for basicL for a static model 
#'
#' @param theta the density independent mortality rate 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @return an **`xds`** model object
#'
#' @export
change_theta = function(theta, xds_obj, s){
  theta = checkIt(theta, xds_obj$nHabitats)
  xds_obj$L_obj[[s]]$theta = theta 
  if (with(xds_obj$L_obj[[s]], ethetasts("theta_obj"))){
    xds_obj$L_obj[[s]]$theta_t = theta
    xds_obj$L_obj[[s]]$f_obj$theta = theta 
  }
  return(xds_obj)
}