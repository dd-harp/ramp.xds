#' @title Change season parameters
#'
#' @description
#' Change parameters for the seasonality function
#'
#' @param X a list with new parameters for bottom, phase, and pw
#' @param xds_obj an **`xds`** model object
#' @param ix the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
#' @keywords internal
change_season = function(X, xds_obj, ix=1){
  UseMethod("change_season", xds_obj$forced_by)
}

#' @title Change season parameters
#'
#' @description
#' Change parameters for the seasonality function
#' when `forced_by = "none"`
#'
#' @inheritParams change_season
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_season.none = function(X, xds_obj, ix=1){
  return(xds_obj)
}

#' @title Change season parameters
#'
#' @description
#' Change parameters for the seasonality function
#' when `forced_by = "Lambda"`
#' @inheritParams change_season
#'
#' @importFrom ramp.func make_function
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_season.Lambda = function(X, xds_obj, ix=1){
  with(xds_obj$L_obj[[ix]]$season_par,
    with(X,{
      xds_obj$L_obj[[ix]]$season_par$pw = pw
      xds_obj$L_obj[[ix]]$season_par$bottom = bottom
      xds_obj$L_obj[[ix]]$season_par$phase = phase
      xds_obj$L_obj[[ix]]$F_season = make_function(xds_obj$L_obj[[ix]]$season_par)
  return(xds_obj)
}))}

#' @title Change season parameters
#'
#' @description
#' Change parameters for the seasonality function
#' when `forced_by = "eir"`
#'
#' @inheritParams change_season
#'
#' @importFrom ramp.func make_function
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_season.eir = function(X, xds_obj, ix=1){
  with(xds_obj$EIR_obj$season_par,
       with(X,{
         xds_obj$EIR_obj$season_par$pw = pw
         xds_obj$EIR_obj$season_par$bottom = bottom
         xds_obj$EIR_obj$season_par$phase = phase
         xds_obj$EIR_obj$F_season = make_function(xds_obj$EIR_obj$season_par)
         return(xds_obj)
}))}
