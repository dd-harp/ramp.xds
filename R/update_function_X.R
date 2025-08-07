
#' Update a function
#'
#' @param X new parameter values
#' @param ix indices or a list
#' @param xds_obj a **`ramp.xds`** xds_obj object
#' @param update a string to dispatch
#' @param s the species index 
#'
#' @returns sum of squared differences
#' @export
update_function_X = function(X, ix, xds_obj, update, s=1){
  class(update) <- update
  UseMethod("update_function_X", update)
}

#' Update Mean Forcing
#'
#' @description Update the mean value
#' that forces a xds_obj. This dispatches
#' on class(xds_obj$forced_by)
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.mean_forcing = function(X, ix, xds_obj, update="mean_forcing", s=1){
  avg   <- get_mean_forcing(xds_obj)
  avg   <- modify_vector_X(X, ix, avg)
  xds_obj <- set_mean_forcing(X, xds_obj, s) 
  return(xds_obj)
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.phase = function(X, ix, xds_obj, update="phase", s=1){
  phase <- get_season_phase(xds_obj)
  phase <- modify_vector_X(X, ix, phase)
  xds_obj <- set_season_phase(phase, xds_obj, s)
  xds_obj <- update_F_season(xds_obj)
  return(xds_obj)
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.amplitude = function(X, ix, xds_obj, update="amplitude", s=1){
  l = length(X)/2
  X_bottom = X[1:l]
  X_pw = X[1:l+l]
  
  bottom <- get_season_bottom(xds_obj)
  bottom <- modify_vector_X(X_bottom, ix, bottom)
  xds_obj <- set_season_bottom(bottom, xds_obj, s)
  
  pw <- get_season_pw(xds_obj)
  pw <- modify_vector_X(X_pw, ix, pw)
  xds_obj <- set_season_pw(pw, xds_obj, s)
  
  xds_obj <- update_F_season(xds_obj)
  return(xds_obj)
}

#' @title Update `F_season` 
#'
#' @description This updates three shape parameters
#' for a function `F_season` using [modify_vector_X] and [update_F_season]
#' 
#' @note This assumes the vector, `X`, has got 
#' three sets of parameters describing three 
#' parameters in a seasonal function with 
#' parameter names phase, bottom, and pw.  
#'  
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.season = function(X, ix, xds_obj, update="season", s=1){
  stopifnot(length(X)%%3 == 0)
  l = length(X)/3
  X_phase = X[1:l]
  X_bottom = X[1:l + l]
  X_pw = X[1:l + 2*l]
  
  phase <- get_season_phase(xds_obj)
  phase <- modify_vector_X(X_phase, ix, phase)
  xds_obj <- set_season_phase(phase, xds_obj, s)
  
  bottom <- get_season_bottom(xds_obj)
  bottom <- modify_vector_X(X_bottom, ix, bottom)
  xds_obj <- set_season_bottom(bottom, xds_obj, s)
  
  pw <- get_season_pw(xds_obj)
  pw <- modify_vector_X(X_pw, ix, phase)
  xds_obj <- set_season_pw(pw, xds_obj, s)
  
  xds_obj <- update_F_season(xds_obj)
  
  return(xds_obj)
}

#' Update a function
#'
#' @inheritParams update_function_X
#'
#' @returns sum of squared differences
#' @export
update_function_X.trend = function(X, ix, xds_obj, update="trend", s=1){
  spline_y <- get_spline(xds_obj)$yy
  spline_y <- modify_vector_X(X, ix, spline_y)
  xds_obj  <- set_spline_y(spline_y, xds_obj)
  xds_obj  <- update_F_trend(xds_obj)
  return(xds_obj)
}


