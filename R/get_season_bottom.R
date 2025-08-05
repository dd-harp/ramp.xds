
#' @title Get the seasonal bottom 
#' 
#' @description
#' Get the parameter that sets the bottom
#' for a seasonally forced model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @returns the bottom parameter(s) 
#' 
#' @export
get_season_bottom = function(xds_obj){
  UseMethod("get_season_bottom", xds_obj$forced_by) 
}


#' @title Get the seasonal bottom 
#' 
#' @description
#' The null case for `get_season_bottom` 
#' 
#' @param xds_obj an **`xds`** object
#'
#' @returns a null value 
#' 
#' @export
get_season_bottom.none = function(xds_obj){
  return(c()) 
}


#' @title Get the seasonal bottom 
#' 
#' @description
#' Get the value(s) that set the bottom
#' for a model that is forced by mosquito emergence 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_bottom.Lambda = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    return(xds_obj$Lpar[[1]]$season_par$bottom)
  } 
  if(xds_obj$nVectorSpecies > 1){
    bottom = list()
    for(i in 1:length(xds_obj$nVectorSpecies)) 
      bottom[[i]] <- xds_obj$Lpar[[i]]$season_par$phase
    return(bottom)
  } 
}

#' @title Get the seasonal bottom 
#' 
#' @description
#' Get the value that sets the bottom
#' for a seasonally forced eir model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_bottom.eir = function(xds_obj){
  return(xds_obj$EIRpar$season_par$bottom)  
}

#' @title Get the seasonal bottom 
#' 
#' @description
#' Get the value that sets bottom
#' for a seasonally forced cohort model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_bottom.cohort = function(xds_obj){
  return(xds_obj$EIRpar$season_par$bottom)  
}

