#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' @param F new shock function
#' @param xds_obj an **`xds`** model object
#' @param s the vector species indeF
#'
#' @return an **`xds`** object
#'
#' @export
change_F_shock = function(F, xds_obj, s=1){
  UseMethod("change_F_shock", xds_obj$forced_by)
}

#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "none"`
#' 
#' @inheritParams change_F_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.none = function(F, xds_obj, s=1){
  return(xds_obj)
}

#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "Lambda"`
#' 
#' @inheritParams change_F_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.Lambda = function(F, xds_obj, s=1){
  xds_obj$L_obj[[s]]$F_shock = F
  return(xds_obj)
}

#' @title Change F_shock
#'
#' @description
#' Change  the shock function
#' when `forced_by = "eir"`
#' 
#' @inheritParams change_F_shock
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_shock.eir = function(F, xds_obj, s=1){
  xds_obj$EIR_obj$shock_par = F
  return(xds_obj)
}
