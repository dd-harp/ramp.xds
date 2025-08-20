
#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' @param s the vector species index
#' 
#' @return seasonal pattern parameters 
#' 
#' @export
get_season = function(xds_obj, s=1){
  UseMethod("get_season", xds_obj$forced_by)
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @inheritParams get_season
#' 
#' @return seasonal pattern parameters 
#' 
#' @export
get_season.none = function(xds_obj, s=1){
   return(c()) 
}

#' @title Get phase 
#' 
#' @description
#' Get the parameter that sets the phase
#' for a seasonally forced model 
#' 
#' @inheritParams get_season 
#' 
#' @returns the phase parameter(s) 
#' 
#' @export
get_season_phase = function(xds_obj, s=1){
  get_season(xds_obj, s)$phase
}

#' @title Get phase 
#' 
#' @description
#' Get the parameter that sets the phase
#' for a seasonally forced model 
#' 
#' @inheritParams get_season 
#' 
#' @returns the phase parameter(s) 
#' 
#' @export
get_season_bottom = function(xds_obj, s=1){
  get_season(xds_obj, s)$bottom
}

#' @title Get pw for seasonality  
#' 
#' @description
#' Get the parameter that sets the phase
#' for a seasonally forced model 
#' 
#' @inheritParams get_season 
#' 
#' @returns the phase parameter(s) 
#' 
#' @export
get_season_pw = function(xds_obj, s=1){
  get_season(xds_obj, s)$pw
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @inheritParams get_season
#' 
#' @return seasonal pattern parameters 
#' 
#' @export
get_season.Lambda = function(xds_obj, s=1){
  xds_obj$Lpar[[s]]$season_par
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters that set 
#' the seasonal pattern
#' 
#' @inheritParams get_season

#' @return seasonal pattern parameters 
#' 
#' @export
get_season.eir= function(xds_obj, s=1){
  xds_obj$EIRpar$season_par
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @inheritParams get_season
#' 
#' @export
get_season.cohort = function(xds_obj, s=1){
  xds_obj$EIRpar$season_par
}