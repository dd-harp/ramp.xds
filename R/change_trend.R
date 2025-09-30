
#' @title Set the interpolating points  
#' 
#' @description
#' Set the interpolating points for F_trend 
#' 
#' @param X the new interpolating points 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_trend = function(X, xds_obj, s=1){
  change_spline(X, xds_obj, s)
} 
  
#' @title Set the interpolating points  
#' 
#' @description
#' Set the interpolating points for F_trend 
#' 
#' @param X new interpolation points, as a list
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_spline = function(X, xds_obj, s=1){
  UseMethod("change_spline", xds_obj$forced_by) 
}

#' @title Set yy 
#' 
#' @description
#' Implement `change_spline` for a model
#' with no forcing
#' 
#' @inheritParams change_spline
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_spline.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set the \eqn{y} value for interpolating points  
#' 
#' @description
#' Set the yy for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_spline
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
change_spline.Lambda = function(X, xds_obj, s=1){
  xds_obj$L_obj[[s]]$trend_par$yy = X$yy
  xds_obj$L_obj[[s]]$trend_par$tt = X$tt
  xds_obj$L_obj[[1]]$F_trend = make_function(xds_obj$L_obj[[1]]$trend_par)
  return(xds_obj)
}

#' @title Set yy 
#' 
#' @description 
#' Set the yy for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_spline
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_spline.eir = function(X, xds_obj, s=1){
  xds_obj$EIR_obj$trend_par$yy = X$yy
  xds_obj$EIR_obj$trend_par$tt = X$tt
  xds_obj$EIR_obj$F_trend <- make_function(xds_obj$EIR_obj$trend_par)
  return(xds_obj)
}

#' @title Set yy 
#' 
#' @description
#' Set the yy parameter to `X`
#' 
#' @param yy new y values for the interpolation points 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_spline_y = function(X, xds_obj, s=1){
  UseMethod("change_spline_y", xds_obj$forced_by) 
}

#' @title Set yy 
#' 
#' @description
#' Implement `change_spline_y` for a model
#' with no forcing
#' 
#' @inheritParams change_spline_y
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_spline_y.none = function(yy, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set the \eqn{y} value for interpolating points  
#' 
#' @description
#' Set the yy for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_spline_y
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
change_spline_y.Lambda = function(yy, xds_obj, s=1){
  stopifnot(length(yy) == length(xds_obj$L_obj[[s]]$trend_par$tt))
  xds_obj$L_obj[[s]]$trend_par$yy = yy
  xds_obj$L_obj[[1]]$F_trend = make_function(xds_obj$L_obj[[1]]$trend_par)
  return(xds_obj)
}

#' @title Set yy 
#' 
#' @description 
#' Set the yy for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams change_spline_y
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_spline_y.eir = function(yy, xds_obj, s=1){
  stopifnot(length(yy) == length(xds_obj$EIR_obj$trend_par$tt))
  xds_obj$EIR_obj$trend_par$yy = yy
  xds_obj$EIR_obj$F_trend <- make_function(xds_obj$EIR_obj$trend_par)
  return(xds_obj)
}
