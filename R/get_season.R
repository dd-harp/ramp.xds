
#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season = function(xds_obj){
  phase <- get_season_phase(xds_obj) 
  bottom <- get_season_bottom(xds_obj) 
  pw <- get_season_pw(xds_obj)
  list(phase=phase, bottom=bottom,  pw=pw)
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_par = function(xds_obj){
  UseMethod("get_season_par", xds_obj$forced_by)
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_par.Lambda = function(xds_obj){
  xds_obj$Lpar[[1]]$season_par
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_par.eir= function(xds_obj){
  xds_obj$EIRpar$season_par
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_par.cohort = function(xds_obj){
  xds_obj$EIRpar$season_par
}