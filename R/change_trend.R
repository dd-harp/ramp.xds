
#' @title Set the interpolating points
#'
#' @description
#' Set the interpolating points for F_trend
#'
#' @param X the new interpolating points
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
change_trend = function(X, xds_obj, s=1){
  change_spline(X, xds_obj, s)
}

#' @title Change spline interpolation points
#'
#' @description
#' Change the interpolation points for a spline function.
#'
#' @param X new interpolation points, as a list
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
change_spline = function(X, xds_obj, s=1){
  UseMethod("change_spline", xds_obj$forced_by)
}

#' @title Change spline interpolation points
#'
#' @description
#' Change the interpolation points for a spline function.
#' when `forced_by = "none"`
#'
#' @inheritParams change_spline
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_spline.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Change spline interpolation points
#'
#' @description
#' Change the interpolation points for a spline function.
#' when `forced_by = "Lambda"`
#'
#' @inheritParams change_spline
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_spline.Lambda = function(X, xds_obj, s=1){
  xds_obj$L_obj[[s]]$trend_par$yy = X$yy
  xds_obj$L_obj[[s]]$trend_par$tt = X$tt
  xds_obj$L_obj[[1]]$F_trend = make_function(xds_obj$L_obj[[1]]$trend_par)
  return(xds_obj)
}

#' @title Change spline interpolation points
#'
#' @description
#' Change the interpolation points for a spline function.
#' when `forced_by = "eir"`
#'
#' @inheritParams change_spline
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_spline.eir = function(X, xds_obj, s=1){
  xds_obj$EIR_obj$trend_par$yy = X$yy
  xds_obj$EIR_obj$trend_par$tt = X$tt
  xds_obj$EIR_obj$F_trend <- make_function(xds_obj$EIR_obj$trend_par)
  return(xds_obj)
}

#' @title Change y values for spline interpolation points
#'
#' @description
#' Set new \eqn{y}-values for the spline interpolating points
#' and update `F_trend`
#'
#' @param yy new y values for the interpolation points
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
change_spline_y = function(yy, xds_obj, s=1){
  UseMethod("change_spline_y", xds_obj$forced_by)
}

#' @title Change y values for spline interpolation points
#'
#' @description
#' Set new \eqn{y}-values for the spline interpolating points
#' and update `F_trend`
#' when `forced_by = "none"`
#'
#' @inheritParams change_spline_y
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_spline_y.none = function(yy, xds_obj, s=1){
  return(xds_obj)
}

#' @title Change y values for spline interpolation points
#'
#' @description
#' Set new \eqn{y}-values for the spline interpolating points
#' and update `F_trend`
#' when `forced_by = "Lambda"`
#'
#' @inheritParams change_spline_y
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_spline_y.Lambda = function(yy, xds_obj, s=1){
  stopifnot(length(yy) == length(xds_obj$L_obj[[s]]$trend_par$tt))
  xds_obj$L_obj[[s]]$trend_par$yy = yy
  xds_obj$L_obj[[1]]$F_trend = make_function(xds_obj$L_obj[[1]]$trend_par)
  return(xds_obj)
}

#' @title Change y values for spline interpolation points
#'
#' @description
#' Set new \eqn{y}-values for the spline interpolating points
#' and update `F_trend`
#' when `forced_by = "eir"`
#'
#' @inheritParams change_spline_y
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_spline_y.eir = function(yy, xds_obj, s=1){
  stopifnot(length(yy) == length(xds_obj$EIR_obj$trend_par$tt))
  xds_obj$EIR_obj$trend_par$yy = yy
  xds_obj$EIR_obj$F_trend <- make_function(xds_obj$EIR_obj$trend_par)
  return(xds_obj)
}

