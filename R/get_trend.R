# X

#' @title Get the trend parameters
#'
#' @description
#' Get the interpolating points
#' for a spline function.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return trend parameters
#'
#' @export
get_trend = function(xds_obj, s=1){
  UseMethod("get_trend", xds_obj$forced_by)
}

#' @title Get the trend parameters
#'
#' @description
#' Get the interpolating points
#' for a spline function.
#'
#' @inheritParams get_trend
#'
#' @return `NULL`
#'
#' @keywords internal
#' @export
get_trend.none = function(xds_obj, s=1){
  return(c())
}

#' @title Get the trend parameters
#'
#' @description
#' Get the interpolating points
#' for a spline function.
#'
#' @inheritParams get_trend
#'
#' @return a list, the value of `trend_par`
#'
#' @keywords internal
#' @export
get_trend.Lambda= function(xds_obj, s=1){
  return(xds_obj$L_obj[[s]]$trend_par)
}

#' @title Get the trend parameters
#'
#' @description
#' Get the interpolating points
#' for a spline function.
#'
#' @inheritParams get_trend
#'
#' @return a list, the value of `trend_par`
#'
#' @keywords internal
#' @export
get_trend.eir= function(xds_obj, s=1){
  return(xds_obj$EIR_obj$trend_par)
}

#' @title Get the trend parameters
#'
#' @description
#' Get the trend parameters for a trivial **MY** model.
#'
#' @inheritParams get_trend
#'
#' @return a list, the value of `trend_par`
#'
#' @keywords internal
#' @export
get_trend.MY = function(xds_obj, s=1){
  return(xds_obj$MY_obj[[s]]$trend_par)
}

#' @title Get spline interpolation points
#'
#' @description
#' Get the interpolating points
#' for a spline function.
#'
#' @param xds_obj an **`xds`** model object
#' @param ix the vector species index
#'
#' @return a [list] containing `tt` and `yy` for the indexed vector species,
#' or `NULL` when there is no forcing
#' @export
get_spline = function(xds_obj, ix=1){
  return(get_spline_s(xds_obj, s=ix))
}

#' @title Get spline interpolation points
#'
#' @description
#' Get the interpolating points
#' for a spline function.
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [list] containing `tt` and `yy`, or `NULL` when there is no forcing
#' @export
get_spline_s = function(xds_obj, s){
  UseMethod("get_spline_s", xds_obj$forced_by)
}

#' @title Get spline interpolation points
#'
#' @description
#' Get spline interpolation points when `forced_by` = "none".
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return `NULL`
#'
#' @keywords internal
#' @export
get_spline_s.none = function(xds_obj, s){
  return(c())
}


#' @title Get spline interpolation points
#'
#' @description
#' Get spline interpolation points when `forced_by` = "Lambda".
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [list] containing `tt` and `yy`
#'
#' @keywords internal
#' @export
get_spline_s.Lambda = function(xds_obj, s=1){
  tt <- xds_obj$L_obj[[s]]$trend_par$tt
  yy <- xds_obj$L_obj[[s]]$trend_par$yy
  return(list(tt=tt, yy=yy))
}

#' @title Get spline interpolation points
#'
#' @description
#' Get spline interpolation points when `forced_by` = "eir".
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [list] containing `tt` and `yy`
#'
#' @keywords internal
#' @export
get_spline_s.eir = function(xds_obj, s=1){
  tt <- xds_obj$EIR_obj$trend_par$tt
  yy <- xds_obj$EIR_obj$trend_par$yy
  return(list(tt=tt, yy=yy))
}
