
#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season = function(xds_obj){
  bottom <- get_season_bottom(xds_obj) 
  phase <- get_season_phase(xds_obj) 
  pw <- get_season_pw(xds_obj)
  list(bottom=bottom, phase=phase, pw=pw)
}
