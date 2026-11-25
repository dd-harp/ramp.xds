# X

#' @title Mosquito Bionomics 
#' 
#' @description
#' 
#' The values of some mosquito bionomic parameters (generically, \eqn{x}) are
#' computed in two stages, with vector control effect sizes computed between them:
#' 
#' + [MosquitoBionomics] computes a *baseline* value, \eqn{x_b}
#' 
#' + Vector control functions compute and store the *net effect size,* \eqn{e_x}
#' 
#' + [VectorControlEffectSizes] computes the realized value \deqn{x = x_b \times e_x}
#'
#' The parameter values can be modified by vector control through one 
#' of two distinct approaches, called *integrated effects* or *independent 
#' effect sizes*. 
#' 
#' In models with integrated effects, \eqn{x_b} could include
#' some effects of vector control, so it may not be correct to call it a 
#' baseline, strictly speaking. 
#'
#' @seealso [xds_info_port]
#' @name xds_info_mosquito_bionomics
NULL

#' @title Check mosquito bionomics (**MY**)
#' @description Currently returns `xds_obj` unchanged; no consistency checks
#' are performed.
#' @inheritParams check_MY
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_MY_bionomics = function(xds_obj, t=0, s){
  return(xds_obj)
}

#' @title Check mosquito bionomics (**L**)
#' @description Currently returns `xds_obj` unchanged; no consistency checks
#' are performed.
#' @inheritParams check_L
#' @return an **`xds`** model object
#' @keywords internal
#' @export
check_L_bionomics = function(xds_obj, t=0, s){
  return(xds_obj)
}

#' @title Compute mosquito bionomic parameters
#' @description This calls [MBionomics] and [LBionomics] for each species.
#'
#' This function computes the bionomic parameters for each vector species,
#' before any vector control effect sizes are applied. In some models, these
#' parameters are computed as a function of resource availability.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
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
#' @description This calls [MEffectSizes] and [LEffectSizes]
#' for each species.
#' This function applies the effect sizes of vector control to the bionomic
#' parameters for each vector species.
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** model object
#' @keywords internal
#' @export
VectorControlEffectSizes <- function(t, y, xds_obj){

  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- MEffectSizes(t, y, xds_obj, s)
    xds_obj <- LEffectSizes(t, y, xds_obj, s)
  }
  return(xds_obj)
}
