
#' @title Set pw, a seasonality shape parameter 
#' 
#' @description
#' Set the pw parameter to `pw`
#' 
#' @param pw a named list with the new parameter values 
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_pw = function(pw, xds_obj, s=1){
  UseMethod("set_season_pw", xds_obj$forced_by) 
}

#' @title Set pw, a seasonality shape parameter 
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
set_season_pw.none = function(pw, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set pw, a seasonality shape parameter 
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
set_season_pw.Lambda = function(pw, xds_obj, s=1){
  stopifnot(length(xds_obj$Lpar[[s]]$season_par$pw) == length(pw))
  xds_obj$Lpar[[s]]$season_par$pw = pw
  xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title Set pw, a seasonality shape parameter 
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
set_season_pw.eir = function(pw, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(pw))
  xds_obj$EIRpar$season_par$pw = pw
  xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title Set pw, a seasonality shape parameter 
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
set_season_pw.cohort = function(pw, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(pw))
  xds_obj$EIRpar$season_par$pw = pw
  xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}  
