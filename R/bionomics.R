#' @title Mosquito Bionomics 
#' 
#' @description
#' The value of a mosquito bionomic parameters gets computed in two stages:
#' 
#' + [MosquitoBionomics] computes the bionomic parameter values;
#' 
#' + [VectorControlEffectSizes] applies the effect sizes of vector control
#' 
#' @section Baseline:
#' The concept of a *baseline* here is not exactly corre 
#'   
#' @section Modified by Control:
#' The concept of a *baseline* is 
#' The value of a parameter 
#' 
#' @name xds_info_mosquito_bionomics
NULL

#' @title Compute mosquito bionomic parameters
#' @description This calls MBionomics and LBionomics for each species.
#'
#' This function computes the bionomic parameters for each vector species,
#' before any vector control effect sizes are applied. In some models, these
#' parameters are computed as a function of resource availability.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return a [list]
#' @keywords internal
#' @export
MosquitoBionomics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- MBionomics(t, y, xds_obj, s)
    xds_obj <- LBionomics(t, y, xds_obj, s)
  }
  return(xds_obj)
}

#' @title Apply vector control effect sizes
#' @description This calls MEffectSizes and LEffectSizes for each species.
#' This function applies the effect sizes of vector control to the bionomic
#' parameters for each vector species.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return a [list]
#' @keywords internal
#' @export
VectorControlEffectSizes <- function(t, y, xds_obj){

  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- MEffectSizes(t, y, xds_obj, s)
    xds_obj <- LEffectSizes(t, y, xds_obj, s)
  }
  return(xds_obj)
}
