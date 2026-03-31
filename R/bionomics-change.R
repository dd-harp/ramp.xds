#' @title Change the blood feeding rate
#'
#' @description Change the blood feeding 
#' rate for a static model
#'
#' @param f the blood feeding rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index 
#'
#' @return an **`xds`** model object
#'
#' @export
change_f = function(f, xds_obj, s){
  xds_obj$MY_obj[[s]]$f = f
  if (with(xds_obj$MY_obj[[s]], exists("f_obj"))){
    xds_obj$MY_obj[[s]]$f_t = f
    xds_obj$MY_obj[[s]]$f_obj$f = f
  }
  return(xds_obj)
}

#' @title Change the mosquito mortality rate
#'
#' @description Change the mosquito mortality
#' rate for a static model
#'
#' @param g the mosquito mortality rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
change_g = function(g, xds_obj, s){
  xds_obj$MY_obj[[s]]$g = g
  if (with(xds_obj$MY_obj[[s]], exists("g_obj"))){
    xds_obj$MY_obj[[s]]$g_t = g
    xds_obj$MY_obj[[s]]$g_obj$g = g
  }
  return(xds_obj)
}

#' @title Change the human fraction
#'
#' @description Change the human fraction
#' for a static model
#'
#' @param q the human fraction
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
change_q = function(q, xds_obj, s){
  xds_obj$MY_obj[[s]]$q = q
  if (with(xds_obj$MY_obj[[s]], exists("q_obj"))){
    xds_obj$MY_obj[[s]]$q_t = q
    xds_obj$MY_obj[[s]]$q_obj$q = q
  }
  return(xds_obj)
}

#' @title Change the mosquito patch emigration rate
#'
#' @description Change the mosquito patch emigration
#' rate for a static model
#'
#' @param sigma the mosquito patch emigration rate
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
change_sigma = function(sigma, xds_obj, s){
  xds_obj$MY_obj[[s]]$sigma = sigma
  if (with(xds_obj$MY_obj[[s]], exists("sigma_obj"))){
    xds_obj$MY_obj[[s]]$sigma_t = sigma
    xds_obj$MY_obj[[s]]$sigma_obj$sigma = sigma
  }
  return(xds_obj)
}

#' @title Change the emigration loss fraction
#'
#' @description Change the emigration loss
#' fraction for a static model
#'
#' @param mu the emigration loss fraction
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
change_mu = function(mu, xds_obj, s){
  xds_obj$MY_obj[[s]]$mu = mu
  if (with(xds_obj$MY_obj[[s]], exists("mu_obj"))){
    xds_obj$MY_obj[[s]]$mu_t = mu
    xds_obj$MY_obj[[s]]$mu_obj$mu = mu
  }
  return(xds_obj)
}

#' @title Change the egg laying rate
#'
#' @description Change the egg laying
#' rate for a static model
#'
#' @param nu the egg laying rate (# batches, per mosquito, per day)
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
change_nu = function(nu, xds_obj, s){
  xds_obj$MY_obj[[s]]$nu = nu
  if (with(xds_obj$MY_obj[[s]], exists("nu_obj"))){
    xds_obj$MY_obj[[s]]$nu_t = nu
    xds_obj$MY_obj[[s]]$nu_obj$nu = nu
  }
  return(xds_obj)
}

#' @title Change the EIP
#'
#' @description Change the extrinsic incubation
#' period for a static model
#'
#' @param eip the extrinsic incubation period
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
change_eip = function(eip, xds_obj, s){
  xds_obj$MY_obj[[s]]$eip = eip
  if (with(xds_obj$MY_obj[[s]], exists("eip_obj"))){
    xds_obj$MY_obj[[s]]$eip_t = eip
    xds_obj$MY_obj[[s]]$eip_obj$eip = eip
  }
  return(xds_obj)
}