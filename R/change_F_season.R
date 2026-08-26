#' @title Change F_season
#'
#' @description
#' Change  the seasonality function. The function `F` should have the form
#' \eqn{F(t,V)}.  If the variable \eqn{V} is not used, it's default value should be set to an empty list. 
#' 
#' @param F a seasonal pattern function
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#' 
#' @seealso [F_one]
#' 
#' @return an **`xds`** object
#'
#' @export
change_F_season = function(F, xds_obj, ix=1){
  UseMethod("change_F_season", xds_obj$forced_by)
}

#' @title Change F_season
#'
#' @description
#' Change  the seasonality function
#' when `forced_by = "none"`
#' 
#' @inheritParams change_F_season
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_season.none = function(F, xds_obj, ix=1){
  return(xds_obj)
}

#' @title Change F_season
#'
#' @description
#' Change  the seasonality function
#' when `forced_by = "Lambda"`
#' 
#' @inheritParams change_F_season
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_season.Lambda = function(F, xds_obj, ix=1){
  xds_obj$L_obj[[ix]]$F_season = F
  return(xds_obj)
}

#' @title Change F_season
#'
#' @description
#' Change  the seasonality function
#' when `forced_by = "eir"`
#' 
#' @inheritParams change_F_season
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_season.eir = function(F, xds_obj, ix=1){
  xds_obj$EIR_obj$F_season = F
  return(xds_obj)
}



#' @title Change F_season
#'
#' @description
#' Change  the season function
#' when `forced_by = "kappa"`
#' 
#' @inheritParams change_F_season
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_season.kappa = function(F, xds_obj, ix=1){
  xds_obj$XH_obj[[ix]]$F_season = F
  return(xds_obj)
}


#' @title Change F_season
#'
#' @description
#' Change  the season function
#' when `forced_by = "MY"`
#' 
#' @inheritParams change_F_season
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
change_F_season.MY = function(F, xds_obj, ix=1){
  xds_obj$MY_obj[[ix]]$F_season = F
  return(xds_obj)
}
