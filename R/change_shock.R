
#' @title Change shock parameters
#'
#' @description
#' Change parameters for the shock function
#' @param shock_par parameters for [make_function]
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
change_shock = function(shock_par, xds_obj, s=1){
  UseMethod("change_shock", xds_obj$forced_by)
}

#' @title Change shock parameters
#'
#' @description
#' Change parameters for the shock function
#' when `forced_by = "none"`
#' @inheritParams change_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_shock.none = function(shock_par, xds_obj, s=1){
  return(xds_obj)
}

#' @title Change shock parameters
#'
#' @description
#' Change parameters for the shock function
#' when `forced_by = "Lambda"`
#' @inheritParams change_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_shock.Lambda = function(shock_par, xds_obj, s=1){
  xds_obj$L_obj[[s]]$shock_par <- shock_par
  xds_obj$L_obj[[s]]$F_shock = make_function(shock_par)
  return(xds_obj)
}

#' @title Change shock parameters
#'
#' @description
#' Change parameters for the shock function
#' when `forced_by = "eir"`
#' @inheritParams change_shock
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_shock.eir = function(shock_par, xds_obj, s=1){
  xds_obj$EIR_obj$shock_par <- shock_par
  xds_obj$EIR_obj$F_shock <- make_function(shock_par)
  return(xds_obj)
}