
#' @title Set pw 
#' 
#' @description
#' Set the pw parameter to `X`
#' 
#' @param X a named list with the new parameter values 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_pw = function(X, xds_obj, s=1){
  UseMethod("set_season_pw", xds_obj$forced_by) 
}

#' @title Set pw 
#' 
#' @description
#' Implement `set_season_pw` for a model
#' with no forcing
#' 
#' @inheritParams set_season_pw
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_pw.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set pw 
#' 
#' @description
#' Set the pw parameter(s) for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_pw
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
set_season_pw.Lambda = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$Lpar[[s]]$season_par$pw) == length(X))
  xds_obj$Lpar[[s]]$season_par$pw = X
  xds_obj$Lpar[[s]]$F_season <- make_function(xds_obj$EIRpar$season_par)
  return(xds_obj)
}

#' @title Set pw 
#' 
#' @description 
#' Set the pw for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_pw
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_pw.eir = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(X))
  xds_obj$EIRpar$season_par$pw = X
  return(xds_obj)
}

#' @title set pw 
#' 
#' @description
#' Set the pw for the eir seasonal pattern for 
#' a `cohort` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_pw
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_pw.cohort = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(X))
  xds_obj$EIRpar$season_par$pw = X
  return(xds_obj)
}  
