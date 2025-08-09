
#' @title Set bottom 
#' 
#' @description
#' Set the bottom parameter to `bottom`
#' 
#' @param bottom the new bottom parameter
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#' @param compile_F if true, call `update_F_season` 
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_bottom = function(bottom, xds_obj, s=1, compile_F=TRUE){
  UseMethod("set_season_bottom", xds_obj$forced_by) 
}

#' @title Set bottom 
#' 
#' @description
#' Implement `set_season_bottom` for a model
#' with no forcing
#' 
#' @inheritParams set_season_bottom
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_bottom.none = function(bottom, xds_obj, s=1, compile_F=TRUE){
  return(xds_obj)
}

#' @title Set bottom 
#' 
#' @description
#' Set the bottom parameter(s) for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_bottom
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
set_season_bottom.Lambda = function(bottom, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$Lpar[[s]]$season_par$bottom) == length(bottom))
  xds_obj$Lpar[[s]]$season_par$bottom = bottom
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title Set bottom 
#' 
#' @description 
#' Set the bottom for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_bottom
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_bottom.eir = function(bottom, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$bottom) == length(bottom))
  xds_obj$EIRpar$season_par$bottom = bottom
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title set bottom 
#' 
#' @description
#' Set the bottom for the eir seasonal pattern for 
#' a `cohort` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_bottom
#' 
#' @export
set_season_bottom.cohort = function(bottom, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$bottom) == length(bottom))
  xds_obj$EIRpar$season_par$bottom = bottom
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}  
