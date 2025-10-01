#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param X a list with new parameters for bottom, phase, and pw 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_season = function(X, xds_obj, s=1){
  UseMethod("change_season", xds_obj$forced_by) 
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param X a list with new parameters for bottom, phase, and pw 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_season.none = function(X, xds_obj, s=1){
  return(xds_obj)
}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param X a list with new parameters for bottom, phase, and pw 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_season.Lambda = function(X, xds_obj, s=1){
  with(xds_obj$L_obj[[s]],
    with(X,{
      xds_obj$L_obj[[s]]$season_par$pw = pw     
      xds_obj$L_obj[[s]]$season_par$bottom = bottom     
      xds_obj$L_obj[[s]]$season_par$phase = phase     
      xds_obj$L_obj[[s]]$F_season = make_function(xds_obj$L_obj[[s]]$season_par)
  return(xds_obj)
}))}

#' @title Get seasonal pattern 
#' 
#' @description
#' Get the parameters set the seasonal pattern
#' 
#' @param X a list with new parameters for bottom, phase, and pw 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return a **`ramp.xds`** model object
#' 
#' @export
change_season.eir = function(X, xds_obj, s=1){
  with(xds_obj$EIR_obj,
       with(X,{
         xds_obj$EIR_obj$season_par$pw = pw     
         xds_obj$EIR_obj$season_par$bottom = bottom     
         xds_obj$EIR_obj$season_par$phase = phase     
         xds_obj$EIR_obj$F_season = make_function(xds_obj$EIR_obj$season_par)
         return(xds_obj)
}))}
