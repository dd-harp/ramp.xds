#' @title Mosquito Bionomics 
#' 
#' @description
#' The value of a mosquito bionomic parameters gets computed in two stages:
#' 
#' + [BaselineBionomics] computes a *baseline* parameter value; 
#' 
#' + [ModifiedBionomics] is the product of the *baseline* and an *effect size*
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

#' @title Set bionomic parameter rates relative to baseline
#' @description This calls MBaseline and LBaseline for each species.
#'
#' This function sets bionomic parameters to their pre-control baseline value, which can later be
#' modified by vector control. In some models, the pre-control baseline is computed in
#' here as a function of resource availability.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return a [list]
#' @keywords internal
#' @export
BaselineBionomics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- MBaseline(t, y, xds_obj, s)
    xds_obj <- LBaseline(t, y, xds_obj, s)
  }
  return(xds_obj)
}

#' @title Set bionomic parameter rates relative to baseline
#' @description This calls Mbionomics and Lbionmics for each species. This function
#' resets bionomic parameters to their pre-control baseline value, which can later be
#' modified by vector control. In some models, the pre-control baseline is computed in
#' here as a function of resource availability.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object
#' @return a [list]
#' @keywords internal
#' @export
ModifiedBionomics <- function(t, y, xds_obj){

  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- MBionomics(t, y, xds_obj, s)
    xds_obj <- LBionomics(t, y, xds_obj, s)
  }
  return(xds_obj)
}
