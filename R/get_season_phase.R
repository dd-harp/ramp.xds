
#' @title Get phase 
#' 
#' @description
#' Get the parameter that sets the phase
#' for a seasonally forced model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @returns the phase parameter(s) 
#' 
#' @export
get_season_phase = function(xds_obj){
  UseMethod("get_season_phase", xds_obj$forced_by) 
}


#' @title Get phase 
#' 
#' @description
#' The null case for `get_season_phase` 
#' 
#' @param xds_obj an **`xds`** object
#'
#' @returns a null value 
#' 
#' @export
get_season_phase.none = function(xds_obj){
  return(c()) 
}


#' @title Get phase 
#' 
#' @description
#' Get the value(s) that set the phase
#' for a model that is forced by mosquito emergence 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_phase.Lambda = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    return(xds_obj$Lpar[[1]]$season_par$phase)
  } 
  if(xds_obj$nVectorSpecies > 1){
    phase = list()
    for(i in 1:length(xds_obj$nVectorSpecies)) 
      phase[[i]] <- xds_obj$Lpar[[i]]$season_par$phase
    return(phase)
  } 
}

#' @title Get phase 
#' 
#' @description
#' Get the value that sets phase
#' for a seasonally forced eir model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_phase.eir = function(xds_obj){
  return(xds_obj$EIRpar$season_par$phase)  
}

#' @title Get phase 
#' 
#' @description
#' Get the value that sets phase
#' for a seasonally forced cohort model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_phase.cohort = function(xds_obj){
  return(xds_obj$EIRpar$season_par$phase)  
}

