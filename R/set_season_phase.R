
#' @title Set phase 
#' 
#' @description
#' Update the phase parameter 
#' 
#' @param phase the new phase parameter
#' @param xds_obj a **`ramp.xds`** model object
#' @param s the vector species index
#' @param compile_F if true, call `update_F_season` 
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_phase = function(phase, xds_obj, s=1, compile_F=TRUE){
  UseMethod("set_season_phase", xds_obj$forced_by) 
}

#' @title Set phase 
#' 
#' @description
#' Implement `set_season_phase` for a model
#' with no forcing
#' 
#' @inheritParams set_season_phase
#' 
#' @return the unmodified **`ramp.xds`** model object 
#' 
#' @export
set_season_phase.none = function(phase, xds_obj, s=1, compile_F=TRUE){
  return(xds_obj)
}

#' @title Set phase 
#' 
#' @description
#' Set the phase for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_phase
#' 
#' @return the **`ramp.xds`** model object with an updated phase parameter
#' 
#' @export
set_season_phase.Lambda = function(phase, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$Lpar[[s]]$season_par$phase) == length(phase))
  xds_obj$Lpar[[s]]$season_par$phase = phase
  xds_obj <- update_F_season(xds_obj, s)
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title Set phase 
#' 
#' @description 
#' Set the phase for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_phase
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_phase.eir = function(phase, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$phase) == length(phase))
  xds_obj$EIRpar$season_par$phase = phase
  xds_obj = update_F_season(xds_obj, s)
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title set phase 
#' 
#' @description
#' Set the phase for the eir seasonal pattern for 
#' a `cohort` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_season_phase
#' 
#' @export
set_season_phase.cohort = function(phase, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$phase) == length(phase))
  xds_obj$EIRpar$season_par$phase = phase
  xds_obj = update_F_season(xds_obj, s)
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}  
