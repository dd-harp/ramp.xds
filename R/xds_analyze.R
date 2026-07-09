#' @title Analyze a Dynamical System
#'
#' @description
#' Besides solving, this function does several housekeeping tasks:
#' + runs [check_models]
#' + sets up the vector of times when outputs are wanted: see [make_times_xde] or [make_times_dts]
#' + sets up \eqn{y_0,} the initial values vector (see [get_inits])
#' + solves the system
#' + parses and attaches outputs (see [parse_outputs])
#'
#' @note
#' The function [xds_solve()] dispatches on `xds_obj$xde`
#'
#' @param xds_obj an **`xds`** model object
#'
#' @export
xds_analyze = function(xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- make_VC(xds_obj, s)
    xds_obj <- compute_VC(xds_obj, s, 1e-6)
  } 
  
#  for(i in 1:xds_obj$nHostSpecies)
#    xds_obj <- make_HTC(xds_obj, i)
#  xds_obj <- make_R0matrix(xds_obj)

  return(xds_obj)  
}