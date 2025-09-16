
#' @title Set bionomic parameter rates relative to baseline
#' @description This calls MBaseline and LBaseline for each species. 
#' 
#' This function sets bionomic parameters to their pre-control baseline value, which can later be
#' modified by vector control. In some models, the pre-control baseline is computed in
#' here as a function of resource availability.
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj a [list]
#' @return a [list]
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
#' @param xds_obj a [list]
#' @return a [list]
#' @export
ModifiedBionomics <- function(t, y, xds_obj){
  
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- MBionomics(t, y, xds_obj, s)
    xds_obj <- LBionomics(t, y, xds_obj, s)
  }
  return(xds_obj)
}
