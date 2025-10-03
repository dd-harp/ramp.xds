
#' @title Set the interpolating points  
#' 
#' @description
#' Set the interpolating points for F_trend 
#' 
#' @param shock_par parameters for [make_function] 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_shock = function(shock_par, xds_obj, s=1){
  UseMethod("change_shock", xds_obj$forced_by) 
}

#' @title Set yy 
#' 
#' @description
#' Implement `change_shock` for a model
#' with no forcing
#' 
#' @inheritParams change_shock
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_shock.none = function(shock_par, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set the \eqn{y} value for interpolating points  
#' 
#' @description
#' 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_shock
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
change_shock.Lambda = function(shock_par, xds_obj, s=1){
  xds_obj$L_obj[[s]]$shock_par <- shock_par 
  xds_obj$L_obj[[s]]$F_shock = make_function(shock_par)
  return(xds_obj)
}

#' @title Set yy 
#' 
#' @description 
#' Set the yy for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_shock
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_shock.eir = function(shock_par, xds_obj, s=1){
  xds_obj$EIR_obj$shock_par <- shock_par 
  xds_obj$EIR_obj$F_shock <- make_function(shock_par)
  return(xds_obj)
}