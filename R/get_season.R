
#' @title Get seasonality parameters
#'
#' @description
#' Get the parameters for the seasonal pattern
#' forcing the model
#'
#' @note Dispatches on `forced_by`
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return seasonal pattern parameters
#'
#' @export
get_season = function(xds_obj, s=1){
  UseMethod("get_season", xds_obj$forced_by)
}

#' @title Get seasonality parameters
#'
#' @description
#' Get seasonality parameters when `forced_by` = "none"
#'
#' @inheritParams get_season
#'
#' @return an empty vector
#'
#' @keywords internal
#' @export
get_season.none = function(xds_obj, s=1){
  return(c())
}

#' @title Get seasonality parameter `phase`
#'
#' @description
#' Get the `phase` parameter for the seasonality function
#'
#' @inheritParams get_season
#'
#' @return the phase parameter(s)
#'
#' @export
get_season_phase = function(xds_obj, s=1){
  get_season(xds_obj, s)$phase
}

#' @title Get seasonality parameter `bottom`
#'
#' @description
#' Get the `bottom` parameter for the seasonality function
#'
#' @inheritParams get_season
#'
#' @return the bottom parameter(s)
#'
#' @export
get_season_bottom = function(xds_obj, s=1){
  get_season(xds_obj, s)$bottom
}

#' @title Get seasonality parameter `pw`
#'
#' @description
#' Get the `pw` parameter for the seasonality function
#'
#' @inheritParams get_season
#'
#' @return the pw parameter(s)
#'
#' @export
get_season_pw = function(xds_obj, s=1){
  get_season(xds_obj, s)$pw
}

#' @title Get seasonality parameters
#'
#' @description
#' Get seasonality parameters when `forced_by` = "Lambda"
#'
#' @inheritParams get_season
#'
#' @return a list, the value of `season_par`
#'
#' @keywords internal
#' @export
get_season.Lambda = function(xds_obj, s=1){
  xds_obj$L_obj[[s]]$season_par
}

#' @title Get seasonality parameters
#'
#' @description
#' Get seasonality parameters when `forced_by` = "eir"
#'
#' @inheritParams get_season
#'
#' @return a list, the value of `season_par`
#'
#' @keywords internal
#' @export
get_season.eir= function(xds_obj, s=1){
  xds_obj$EIR_obj$season_par
}

#' @title Get seasonality parameters
#'
#' @description
#' Get seasonality parameters when `forced_by` = "MY"
#'
#' @inheritParams get_season
#'
#' @return a list, the value of `season_par`
#'
#' @keywords internal
#' @export
get_season.MY = function(xds_obj, s=1){
  xds_obj$MY_obj[[s]]$season_par
}