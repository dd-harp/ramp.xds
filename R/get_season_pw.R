
#' @title Get the seasonal pw 
#' 
#' @description
#' Get the pw parameter
#' for a seasonally forced model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @returns the pw parameter(s) 
#' 
#' @export
get_season_pw = function(xds_obj){
  UseMethod("get_season_pw", xds_obj$forced_by) 
}


#' @title Get the seasonal pw 
#' 
#' @description
#' The null case for `get_season_pw` 
#' 
#' @param xds_obj an **`xds`** object
#'
#' @returns a null value 
#' 
#' @export
get_season_pw.none = function(xds_obj){
  return(c()) 
}


#' @title Get the seasonal pw 
#' 
#' @description
#' Get the pw parameter(s) 
#' for a model that is seasonally forced by mosquito emergence 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_pw.Lambda = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    return(xds_obj$Lpar[[1]]$season_par$pw)
  } 
  if(xds_obj$nVectorSpecies > 1){
    pw = list()
    for(i in 1:length(xds_obj$nVectorSpecies)) 
      pw[[i]] <- xds_obj$Lpar[[i]]$season_par$phase
    return(pw)
  } 
}

#' @title Get the seasonal pw 
#' 
#' @description
#' Get the pw parameter(s) 
#' for a seasonally forced eir model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_pw.eir = function(xds_obj){
  return(xds_obj$EIRpar$season_par$pw)  
}

#' @title Get the seasonal pw 
#' 
#' @description
#' Get the value that sets pw
#' for seasonally forcing on the daily 
#' EIR for a cohort model 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_season_pw.cohort = function(xds_obj){
  return(xds_obj$EIRpar$season_par$pw)  
}

