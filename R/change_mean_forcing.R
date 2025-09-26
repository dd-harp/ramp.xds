
#' @title Set mean forcing 
#' 
#' @description
#' Set the mean forcing parameter to `X`
#' 
#' @param X the new mean forcing parameter
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_mean_forcing = function(X, xds_obj, s=1){
  UseMethod("change_mean_forcing", xds_obj$forced_by) 
}

#' @title Set mean forcing 
#' 
#' @description
#' Implement `change_mean_forcing` for a model
#' with no forcing
#' 
#' @inheritParams change_mean_forcing
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_mean_forcing.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set mean forcing 
#' 
#' @description
#' Set `Lambda` to `X` for the \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_mean_forcing
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
change_mean_forcing.Lambda = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$L_obj[[s]]$Lambda) == length(X))
  xds_obj$L_obj[[s]]$Lambda = X
  return(xds_obj)
}

#' @title Set mean forcing 
#' 
#' @description 
#' Set the mean daily EIR for an `eir` model
#' 
#' @inheritParams change_mean_forcing
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_mean_forcing.eir = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIR_obj$eir) == length(X))
  xds_obj$EIR_obj$eir = X
  return(xds_obj) 
}


