#' @title Change F_trend
#'
#' @description
#' Change  the trend function
#' @param F new trend function
#' @param xds_obj an **`xds`** model object
#' @param s the vector species indeF
#'
#' @return an **`xds`** object
#'
#' @export
change_F_trend = function(F, xds_obj, s=1){
  UseMethod("change_F_trend", xds_obj$forced_by)
}

#' @title Change F_trend
#'
#' @description
#' Change  the trend function
#' when `forced_by = "none"`
#' 
#' @inheritParams change_F_trend
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_trend.none = function(F, xds_obj, s=1){
  return(xds_obj)
}

#' @title Change F_trend
#'
#' @description
#' Change  the trend function
#' when `forced_by = "Lambda"`
#' 
#' @inheritParams change_F_trend
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_trend.Lambda = function(F, xds_obj, s=1){
  xds_obj$L_obj[[s]]$F_trend = F
  return(xds_obj)
}

#' @title Change F_trend
#'
#' @description
#' Change  the trend function
#' when `forced_by = "eir"`
#' 
#' @inheritParams change_F_trend
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_trend.eir = function(F, xds_obj, s=1){
  xds_obj$EIR_obj$trend_par = F
  return(xds_obj)
}
