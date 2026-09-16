#' @title Setup births
#' 
#' @description
#' Configure the births function
#' 
#' @param name the name of the births function
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** object
#' 
#' @export
setup_births = function(name, xds_obj, i=1){
  births = list() 
  class(births) = name 
  births$name = name
  xds_obj$XH_obj[[i]]$birth_obj = births
  return(xds_obj)
}

#' @title Setup demographic matrix
#' 
#' @description
#' Setup the demographic matirx
#' 
#' @param name a setup method
#' @param xds_obj an **`xds`** model object
#' @param options options list (overrides command line arguments)
#' @param i the species index
#'
#' @return an **`xds`** object
#' 
#' @export
setup_mortality_matrix = function(name, xds_obj, options, i=1){
  class(name) <- name 
  UseMethod("setup_mortality_matrix", name)
}

#' @title Setup demographic matrix
#' 
#' @description
#' Setup the demographic matirx
#' 
#' @param name a setup method
#' @param xds_obj an **`xds`** model object
#' @param options options list (overrides command line arguments)
#' @param i the species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_mortality_matrix.default = function(name, xds_obj, options, i=1){
  xds_obj$XH_obj[[i]]$D_matrix = diag(0, xds_obj$nStrata[i]) 
  return(xds_obj)
}

#' @title Human (or Host) population birth rate
#'
#' @description This method dispatches on the type of xds_obj$Hpar$Births
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births <- function(t, xds_obj, i){
  UseMethod("Births", xds_obj$XH_obj[[i]]$birth_obj)
}

#' @title Human (or Host) population birth rate
#'
#' @description a function
#'
#' @inheritParams Births
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births.zero <- function(t, xds_obj, i){return(0*t)}


