# X

#' @title Get the mosquito demographic matrix (**MY**)
#' 
#' @description Return the stored mosquito demographic matrix, \eqn{\Omega},
#' for the selected species.
#' 
#' @note The matrix is stored at `xds_obj$MY_obj[[s]]$Omega`.
#' This function does not recompute the matrix or modify the model object.
#' @seealso [get_Upsilon], [change_Omega], [xds_info_mosquito_demography]
#'  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix] of dimensions `nPatches` by `nPatches`,
#' or `NULL` if no matrix is stored
#'
#' @export
get_Omega = function(xds_obj, s=1){
  return(xds_obj$MY_obj[[s]]$Omega) 
}

#' @title Get the EIP survival-and-dispersal matrix (**MY**)
#' 
#' @description Return the stored matrix describing mosquito survival and
#' dispersal through the EIP, \eqn{\Upsilon}, for the selected species.
#' 
#' @note The matrix is stored at `xds_obj$MY_obj[[s]]$Upsilon`.
#' This function does not recompute the matrix or modify the model object.
#' @seealso [get_Omega], [change_Upsilon], [xds_info_mosquito_demography]
#'  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix] of dimensions `nPatches` by `nPatches`,
#' or `NULL` if no matrix is stored
#'
#' @export
get_Upsilon = function(xds_obj, s=1){
  return(xds_obj$MY_obj[[s]]$Upsilon) 
}
#' @title Get orbits (**MY**)
#' 
#' @description Return the saved, parsed orbits (trajectories) for **MY**
#' for the selected species after solving.
#' @note Orbits are stored at `xds_obj$outputs$orbits$MY[[s]]`.
#' The returned list includes `time` from `xds_obj$outputs$time`.
#' This function does not parse outputs
#' or modify the model object.
#' @seealso [parse_MY_orbits], [xds_solve]
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [list] of module-specific orbits (trajectories) with a `time` vector
#'
#' @export
get_MY_orbits = function(xds_obj, s=1){

  got = xds_obj$outputs$orbits$MY[[s]]
  got$time = xds_obj$outputs$time

  return(got)
}
