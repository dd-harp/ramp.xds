#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param X a list with new parameters for bottom, phase, and pw 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season = function(X, xds_obj, s=1){
  
  if(length(X$bottom) == xds_obj$nPatches)
    xds_obj <- set_season_bottom(X$bottom, xds_obj, s, compile_F=FALSE) 
  
  if(length(X$phase) == xds_obj$nPatches)
    xds_obj <- set_season_phase(X$phase, xds_obj, s, compile_F=FALSE) 
  
  if(length(X$pw) == xds_obj$nPatches)
    xds_obj <- set_season_pw(X$pw, xds_obj, s, compile_F=FALSE)
  
  xds_obj <- update_F_season(xds_obj, s) 
  
  return(xds_obj)
}


#' @title Set bottom 
#' 
#' @description
#' Set the bottom parameter to `bottom`
#' 
#' @param bottom the new bottom parameter
#' @param xds_obj an **`xds`** model object
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


#' @title Set phase 
#' 
#' @description
#' Update the phase parameter 
#' 
#' @param phase the new phase parameter
#' @param xds_obj an **`xds`** model object
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
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}  


#' @title Set pw, a seasonality shape parameter 
#' 
#' @description
#' Set the pw parameter to `pw`
#' 
#' @param pw a named list with the new parameter values 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species inde
#' @param compile_F if true, call `update_F_season` 
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_season_pw = function(pw, xds_obj, s=1, compile_F=TRUE){
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
set_season_pw.none = function(pw, xds_obj, s=1, compile_F=TRUE){
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
set_season_pw.Lambda = function(pw, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$Lpar[[s]]$season_par$pw) == length(pw))
  xds_obj$Lpar[[s]]$season_par$pw = pw
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
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
set_season_pw.eir = function(pw, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(pw))
  xds_obj$EIRpar$season_par$pw = pw
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
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
set_season_pw.cohort = function(pw, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(pw))
  xds_obj$EIRpar$season_par$pw = pw
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}  


#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season = function(xds_obj, s=1){
  UseMethod("update_F_season", xds_obj$forced_by)     
}

#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @inheritParams update_F_season 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season.Lambda = function(xds_obj, s=1){
  xds_obj$Lpar[[s]]$F_season = make_function(xds_obj$Lpar[[s]]$season_par)
  return(xds_obj)
}

#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @inheritParams update_F_season 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season.eir = function(xds_obj, s=1){
  xds_obj$EIRpar$F_season <- make_function(xds_obj$EIRpar$season_par)
  return(xds_obj)
}

#' @title Update the seasonality function 
#' 
#' @description Update `F_season` 
#' 
#' @inheritParams update_F_season 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_season.cohort = function(xds_obj, s=1){
  xds_obj$EIRpar$F_season <- make_function(xds_obj$EIRpar$season_par)
  return(xds_obj)
}
