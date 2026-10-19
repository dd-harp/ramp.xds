#' @title Forcing in **`ramp.xds`**
#' 
#' @description
#' 
#' In **`ramp.xds`**, forcing is implemented through *ports* and *junctions,* or through exogenous
#' forcing of dynamical terms by `trivial` modules (see [xds_info_trivial_forcing]). 
#' 
#' + Parameter or terms that could be time-varying are implemented as *ports.* There are several
#' kinds of ports: 
#' 
#'    - mosquito bionomic ports set a baseline that can be modified by vector control;
#'     
#'    - pharmaceutical ports are used to model mass treatment; 
#'    
#'    - terms and parameters affecting blood feeding, transmission, exposure or habitat dynamics are implemented as ports; and 
#'    
#'    - other exogenous variables are implemented as ports.
#'    
#' + Junctions are functions that activate ports (see [xds_info_port]): 
#' 
#'    - [`Forcing`] handles weather, hydrology, system shocks, and development;
#'    
#'    - [`Importation`]  sets up time away, the travel EIR, visitors, and their infectiousness;
#'    
#'    - [`Health`] sets up variables associated with health systems; 
#'    
#'    - Vector control is implemented in two parts; 
#'    
#'    - [`Resources`] sets up variables associated with mosquito ecology that are not part of blood feeding or egg laying; 
#'    
#'    - Four junctions implement mosquito bionomics: two set the baseline (for adult and immature mosquitoes) and two modify the baseline with vector control *effect sizes.*
#' 
#' @seealso [xds_info_trivial_forcing]; [xds_info_vector_control]; [xds_info_health]
#' @name xds_info_forcing 
NULL

#' @title Forcing
#'
#' @description
#' A junction for weather, hydrology, system shocks,
#' and development. In **`ramp.xds`**, the junction is
#' turned off. 
#'
#' @note For advanced setup options, see [**`ramp.forcing`**](https://dd-harp.github.io/ramp.forcing/) 
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
Forcing = function(t, xds_obj){
  UseMethod("Forcing", xds_obj$forcing_obj)
}

#' @title Forcing
#'
#' @description
#' Implements no `Forcing`, the default. 
#' 
#' @note Dynamic forcing is implemented in **`ramp.forcing`** 
#' 
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
Forcing.none = function(t, xds_obj){
  return(xds_obj)
}

#' @title Set up forcing
#' 
#' @description Set up the `Forcing` junction and turn it off.
#' 
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_forcing_object = function(xds_obj){
  forcing <- list()
  class(forcing) <- 'none'
  forcing$name <- "Junction: Forcing"
  xds_obj$forcing_obj <- forcing
  return(xds_obj)
}

