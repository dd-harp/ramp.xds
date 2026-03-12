#' @title The Human Population 
#' 
#' @description
#' The human population (or other host population) and its structure, size and behavior
#' are critical aspects of transmission. A human population is stratified to match
#' the patch dynamics: each stratum resides in some patch. 
#' 
#' \describe{
#'   \item{`residence`}{a vector: the patch index where each human stratum lives}
#'   \item{`nStrata`}{`nStrata = length(residence)`}
#'   \item{`HPop`}{human population size: `length(HPop) = nStrata`}
#' }
#' 
#' @section Human Population Size, \eqn{H} 
#' 
#' @section The Residency Matrix: 
#' 
#' @name human_populations
NULL

#' @title Human (or Host) Population Birth Rate
#'
#' @description This method dispatches on the type of xds_obj$Hpar$Births
#'
#' @param t current simulation time
#' @param H population density
#' @param births the birth function object
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births <- function(t, H, births){
  UseMethod("Births", births)
}

#' @title Human (or Host) Population Birth Rate
#'
#' @description a function
#'
#' @inheritParams Births
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births.zero <- function(t, H, births="zero"){return(0*t)}


