#' @title **`ramp.xds`**: Forcing 
#' 
#' @description
#' In **`ramp.xds`**, forcing is implemented in several ways:
#' + through `trivial` modules: see [xds_info_trivial_forcing]
#' + a library of modulesas functional responses to other variables: models are 
#' developed by activiting the [Forcing] junction 
#' + through malaria control: 
#'    - see [xds_info_vector_control] 
#'    - see [xds_info_health] 
#' 
#' 
#' @seealso [xds_info_trivial_forcing] 
#' @name xds_info_forcing 
NULL

#' @title Exogenous Forcing
#'
#' @description
#' A junction for exogenous forcing
#' by weather and hydrology. Non-trivial
#' examples are in the satellite package
#' `ramp.forcing`
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
Forcing = function(t, xds_obj){
  UseMethod("Forcing", xds_obj$forcing_obj)
}

#' @title Exogenous Forcing
#'
#' @description
#' After basic setup, no exogenous
#' variables are set up. `Forcing` returns
#' the unmodified **`xds`** object.
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
Forcing.none = function(t, xds_obj){
  return(xds_obj)
}

#' @title Setup the Junction for Exogenous Forcing
#' @description This sets up the `none` option
#' for exogenous forcing: no forcing.
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_forcing_object = function(xds_obj){
  forcing <- list()
  class(forcing) <- 'none'
  forcing$name <- "Junction: Exogneous Forcing"
  forcing$ports <- "Ports: Rainfall, Temperature, Humidity"
  xds_obj$forcing_obj <- forcing
  return(xds_obj)
}

