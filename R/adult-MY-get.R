
#' @title Get orbits (**MY**)
#' 
#' @description After solving, the orbits
#' are parsed and stored. Get orbits retrieves
#' the parsed outputs for the **MY** module
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the orbits for the **MY** component
#'
#' @export
get_MY_orbits = function(xds_obj, s=1){

  got = xds_obj$outputs$orbits$MY[[s]]
  got$time = xds_obj$outputs$time

  return(got)
}

#' @title Get Omega 
#' 
#' @description Return the demographic
#' matrix, called \eqn{\Omega}. 
#' 
#' @seealso [xds_info_mosquito_demography] 
#'  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the demographic matrix 
#'
#' @export
get_Omega = function(xds_obj, s=1){
  return(xds_obj$MY_obj[[s]]$Omega) 
}

#' @title Get Upsilon 
#' 
#' @description Return \eqn{\Upsilon}, 
#' a matrix describing survival and dispersal
#' through the EIP. 
#' 
#' @seealso [xds_info_mosquito_demography] 
#'  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the demographic matrix 
#'
#' @export
get_Upsilon = function(xds_obj, s=1){
  return(xds_obj$MY_obj[[s]]$Upsilon) 
}