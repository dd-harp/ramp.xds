#' @title Setup Seasonal Forcing
#'
#' @description
#' Setup seasonal forcing
#'
#' @param X a seasonality function object
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @return an **`xds`** object
#'
#' @export
#' @keywords internal
setup_season = function(X, xds_obj, ix=1){
  UseMethod("setup_season", xds_obj$forced_by)
}

#' @title Change season parameters
#'
#' @description Setup season for the NULL case
#'
#' @inheritParams setup_season
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
setup_season.none = function(X, xds_obj, ix=1){
  return(xds_obj)
}

#' @title Change season parameters
#'
#' @description
#' Change parameters for the seasonality function
#' when `forced_by = "Lambda"`
#' @inheritParams setup_season
#'
#' @importFrom ramp.func make_function
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
setup_season.Lambda = function(X, xds_obj, ix=1){
  xds_obj$L_obj[[ix]]$season_par <- X
  xds_obj$L_obj[[ix]]$F_season = make_function(X)
  return(xds_obj)
}

#' @title Change season parameters
#'
#' @description
#' Change parameters for the seasonality function
#' when `forced_by = "eir"`
#' @inheritParams setup_season
#'
#' @importFrom ramp.func make_function
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
setup_season.eir = function(X, xds_obj, ix=1){
  xds_obj$EIR_obj$season_par <- X
  xds_obj$EIR_obj$F_season <- make_function(X)
  return(xds_obj)
}
