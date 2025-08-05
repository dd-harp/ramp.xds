
#' @title Get mean forcing 
#' 
#' @description
#' Get the parameter(s) that set mean forcing
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @returns the mean parameter for forcing
#'  
#' @export
get_mean_forcing = function(xds_obj){
  UseMethod("get_mean_forcing", xds_obj$forced_by) 
}

#' @title Get mean forcing 
#' 
#' @description
#' Return null for models without forcing 
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_mean_forcing.none = function(xds_obj){
  return(c()) 
}

#' @title Get mean forcing 
#' 
#' @description
#' Get `Lambda`, the mean emergence
#' rate(s) \eqn{\Lambda}, for a model 
#' forced by adult mosquito emergence
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_mean_forcing.Lambda = function(xds_obj){
  if(xds_obj$nVectorSpecies == 1){
    return(xds_obj$Lpar[[1]]$Lambda)
  } else { 
    Lambda = list() 
    for(s in 1:length(xds_obj$nVectorSpecies)) 
      Lambda[[s]] <- xds_obj$Lpar[[s]]$Lambda 
    return(Lambda)
  } 
}

#' @title Get mean forcing 
#' 
#' @description
#' Get the mean daily EIR for an `eir` model
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_mean_forcing.eir = function(xds_obj){
  return(xds_obj$EIRpar$eir)  
}

#' @title Get mean forcing 
#' 
#' @description
#' Get the mean daily EIR for 
#' a `cohort` model
#' 
#' @param xds_obj an **`xds`** object
#' 
#' @export
get_mean_forcing.cohort = function(xds_obj){
  return(xds_obj$EIRpar$cohort)  
}

