
#' Rebuild Forcing Functions 
#'
#' @description
#' Rebuild forcing functions using `make_function`:
#' + `F_season` is made from `season_par` 
#' + `F_trend` is made from `trend_par` 
#' + `F_shock` is made from `shock_par` 
#'  
#' @seealso [readXDS()]
#'  
#' @param xds_obj an **`xds`** model object
#' @param ix the species index 
#' @keywords internal 
#'
#' @returns an **`xds`** model object
#' @export
rebuild_forcing_functions = function(xds_obj, ix){
  UseMethod("rebuild_forcing_functions", xds_obj$forced_by)
}

#' Rebuild Forcing Functions 
#' 
#' @description
#' Rebuild forcing functions on the EIR object using `make_function`:
#' + `F_season` is made from `season_par` 
#' + `F_trend` is made from `trend_par` 
#' + `F_shock` is made from `shock_par` 
#'  
#' @inheritParams rebuild_forcing_functions 
#' @keywords internal 
#'
#' @returns an **`xds`** model object
#' @export
rebuild_forcing_functions.eir = function(xds_obj, ix=1){
  with(xds_obj$EIR_obj,{
    xds_obj$EIR_obj$F_season = make_function(season_par) 
    xds_obj$EIR_obj$F_trend = make_function(trend_par) 
    xds_obj$EIR_obj$F_age = make_function(age_par)
    xds_obj$EIR_obj$F_shock = make_function(shock_par) 
    return(xds_obj)
})}

#' Rebuild Forcing Functions 
#' 
#' @description
#' Rebuild forcing functions on the L object using `make_function`:
#' + `F_season` is made from `season_par` 
#' + `F_trend` is made from `trend_par` 
#' + `F_shock` is made from `shock_par` 
#'  
#' @inheritParams rebuild_forcing_functions 
#' @keywords internal 
#'
#' @returns an **`xds`** model object
#' @export
rebuild_forcing_functions.Lambda = function(xds_obj, ix){
  with(xds_obj$L_obj[[ix]],{
    xds_obj$L_obj[[ix]]$F_season = make_function(season_par) 
    xds_obj$L_obj[[ix]]$F_trend = make_function(trend_par) 
    xds_obj$L_obj[[ix]]$F_shock = make_function(shock_par) 
    return(xds_obj)
  })}

#' Rebuild Forcing Functions 
#' 
#' @description
#' Rebuild forcing functions on the MY object using `make_function`:
#' + `F_season` is made from `season_par` 
#' + `F_trend` is made from `trend_par` 
#' + `F_shock` is made from `shock_par` 
#'  
#' @inheritParams rebuild_forcing_functions 
#' @keywords internal 
#'
#' @returns an **`xds`** model object
#' @export
rebuild_forcing_functions.MY = function(xds_obj, ix){
  with(xds_obj$MY_obj[[ix]],{
    xds_obj$MY_obj[[ix]]$F_season = make_function(season_par) 
    xds_obj$MY_obj[[ix]]$F_trend = make_function(trend_par) 
    xds_obj$MY_obj[[ix]]$F_shock = make_function(shock_par) 
    return(xds_obj)
  })}

#' Rebuild Forcing Functions 
#' 
#' @description
#' Rebuild forcing functions on the X object using `make_function`:
#' + `F_season` is made from `season_par` 
#' + `F_trend` is made from `trend_par` 
#' + `F_shock` is made from `shock_par` 
#'  
#' @inheritParams rebuild_forcing_functions 
#' @keywords internal 
#'
#' @returns an **`xds`** model object
#' @export
rebuild_forcing_functions.XH = function(xds_obj, ix){
  with(xds_obj$XH_obj[[ix]],{
    xds_obj$XH_obj[[ix]]$F_season = make_function(season_par) 
    xds_obj$XH_obj[[ix]]$F_trend = make_function(trend_par) 
    xds_obj$XH_obj[[ix]]$F_shock = make_function(shock_par) 
    xds_obj$XH_obj[[ix]]$H_trend = make_function(H_trend_par) 
    return(xds_obj)
  })}