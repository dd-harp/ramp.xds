
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
set_trend = function(X, xds_obj, s=1){
  set_spline(X, xds_obj, s)
} 
  
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
set_spline = function(X, xds_obj, s=1){
  UseMethod("set_spline", xds_obj$forced_by) 
}

#' @title Set yy 
#' 
#' @description
#' Implement `set_spline` for a model
#' with no forcing
#' 
#' @inheritParams set_spline
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_spline.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set the \eqn{y} value for interpolating points  
#' 
#' @description
#' Set the yy for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_spline
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
set_spline.Lambda = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$Lpar[[s]]$trend_par$yy) == length(X$yy))
  stopifnot(length(xds_obj$Lpar[[s]]$trend_par$tt) == length(X$tt))
  xds_obj$Lpar[[s]]$trend_par$yy = X$yy
  xds_obj$Lpar[[s]]$trend_par$tt = X$tt
  return(xds_obj)
}

#' @title Set yy 
#' 
#' @description 
#' Set the yy for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_spline
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_spline.eir = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$trend_par$yy) == length(X$yy))
  stopifnot(length(xds_obj$EIRpar$trend_par$tt) == length(X$tt))
  xds_obj$EIRpar$trend_par$yy = X$yy
  xds_obj$EIRpar$trend_par$tt = X$tt
  return(xds_obj)
}

#' @title set yy 
#' 
#' @description
#' Set the yy for the eir seasonal pattern for 
#' a `cohort` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_spline
#' 
#' @export
set_spline.cohort = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$trend_par$yy) == length(X$yy))
  stopifnot(length(xds_obj$EIRpar$trend_par$tt) == length(X$tt))
  xds_obj$EIRpar$trend_par$yy = X$yy
  xds_obj$EIRpar$trend_par$tt = X$tt
  return(xds_obj)
}  

#' @title Set yy 
#' 
#' @description
#' Set the yy parameter to `X`
#' 
#' @param X the new yy parameter
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_spline_y = function(X, xds_obj, s=1){
  UseMethod("set_spline_y", xds_obj$forced_by) 
}

#' @title Set yy 
#' 
#' @description
#' Implement `set_spline_y` for a model
#' with no forcing
#' 
#' @inheritParams set_spline_y
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_spline_y.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Set the \eqn{y} value for interpolating points  
#' 
#' @description
#' Set the yy for the seasonal pattern for the 
#' \eqn{s^{th}} species
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_spline_y
#' 
#' @return the **`ramp.xds`** model object
#' 
#' @export
set_spline_y.Lambda = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$Lpar[[s]]$trend_par$yy) == length(X))
  xds_obj$Lpar[[s]]$trend_par$yy = X
  return(xds_obj)
}

#' @title Set yy 
#' 
#' @description 
#' Set the yy for the seasonal pattern for 
#' an `eir` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_spline_y
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
set_spline_y.eir = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$trend_par$yy) == length(X))
  xds_obj$EIRpar$trend_par$yy = X
  return(xds_obj)
}

#' @title set yy 
#' 
#' @description
#' Set the yy for the eir seasonal pattern for 
#' a `cohort` model 
#' and return the **`ramp.xds`** model object
#' 
#' @inheritParams set_spline_y
#' 
#' @export
set_spline_y.cohort = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$trend_par$yy) == length(X))
  xds_obj$EIRpar$trend_par$yy = X
  return(xds_obj)
}  


#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend = function(xds_obj, s=1){
  UseMethod("update_F_trend", xds_obj$forced_by)     
}

#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @inheritParams update_F_trend 
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend.Lambda = function(xds_obj, s=1){
  xds_obj$Lpar[[1]]$F_trend = make_function(xds_obj$Lpar[[1]]$trend_par)
  return(xds_obj)
}

#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend.eir = function(xds_obj, s=1){
  xds_obj$EIRpar$F_trend <- make_function(xds_obj$EIRpar$trend_par)
  return(xds_obj)
}

#' @title Update the trend function 
#' 
#' @description Update `F_trend`
#' 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'  
#' @return a **`ramp.xds`** model object
#' 
#' @export
update_F_trend.cohort = function(xds_obj, s=1){
  xds_obj$EIRpar$F_trend <- make_function(xds_obj$EIRpar$trend_par)
  return(xds_obj)
}
