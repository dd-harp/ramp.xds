
#' @title Setup the Habitats Object 
#' 
#' @description 
#' During basic setup, the habitats membership
#' matrix is set up. 
#' 
#' The `habitat_obj` handles:
#' + dynamically changing habitat weights
#' + availability of bad habitats
#' + with multiple vector species, weights can be assigned to bad habitats 
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
setup_habitat_object = function(xds_obj){
  habs <- list()
  class(habs) <- "setup"
  Otv <- One_tV 
  class(Otv) <- "na"
  Ztv <- Zero_tV 
  class(Ztv) <- "na"
  habs$F_habitat_weights <- Otv 
  habs$F_bad_habitat <- Ztv 
  habs$F_bad_hab_wts <- Ztv 
  xds_obj$ML_interface$habitat_obj <- list()
  class(xds_obj$ML_interface$habitat_obj) <- "setup" 
  xds_obj$ML_interface$habitat_obj[[1]] <- habs 
  return(xds_obj)
}

#' @title Change Habitat Search Weights
#' @description Set the search weights, \eqn{\omega}, for a set of aquatic habitats
#' 
#' @param wts habitat search weights
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_habitat_weights = function(wts, xds_obj, s=1){
  stopifnot(length(wts) == xds_obj$nHabitats)
  xds_obj$ML_interface$search_weights[[s]] = wts 
  class(xds_obj$ML_interface$habitat_obj) <- 'setup'
  return(xds_obj)
}

#' @title Change Habitat Search Weights
#' @description Set the search weights, \eqn{\omega}, for a set of aquatic habitats
#' @param Qbad availability of bad habitat 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an `xds` object
#' @export
change_bad_habitat = function(Qbad, xds_obj, s=1){
  stopifnot(length(Qbad) == xds_obj$nHabitats)
  xds_obj$ML_interface$Qbad[[s]] = Qbad 
  xds_obj$ML_interface$habitat_obj <- trigger_setup(xds_obj$ML_interface$habitat_obj) 
  return(xds_obj)
}

#' @title Habitat Dynamics 
#' 
#' @description This function modifies search weights for
#' habitats and availability of bad habitat 
#' 
#' @param t current simulation time
#' @param y state variables 
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
HabitatDynamics <- function(t, y, xds_obj) {
  UseMethod("HabitatDynamics", xds_obj$ML_interface$habitat_obj)
}

#' @title Habitat Dynamics 
#' 
#' @description This function modifies search weights for
#' habitats and availability of bad habitat 
#' 
#' @inheritParams HabitatDynamics 
#' 
#' @return an **`xds`** object
#' 
#' @export
#' @keywords internal
HabitatDynamics.setup<- function(t, y, xds_obj) {
  class(xds_obj$ML_interface$habitat_obj) <- 'static'
  xds_obj$ML_interface <- trigger_setup(xds_obj$ML_interface) 
  return(xds_obj)
}

#' @title Habitat Dynamics 
#' 
#' @description This function modifies search weights for
#' habitats and availability of bad habitat 
#' 
#' @inheritParams HabitatDynamics 
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
HabitatDynamics.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Habitat Dynamics 
#' 
#' @description This function modifies search weights for
#' habitats and availability of bad habitat 
#' 
#' @inheritParams HabitatDynamics 
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
HabitatDynamics.dynamic <- function(t, y, xds_obj) {
  return(habitat_dynamics(t, y, xds_obj)) 
}

#' @title Habitats Dynamics 
#' 
#' @description 
#' 
#' A function `F_habitat_weights` computes `TiSp_frac_searching` and  
#' this function sets `TiSp_frac_here.` 
#' 
#' @note The quantity `TiSp_frac_here` is part of the `ML_interface`
#' It is used to compute the Time at Risk matrix, and it is also 
#' used to weight local exposure *vs.*  searching exposure.  
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
habitat_dynamics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){

    F_wts <- xds_obj$ML_interface$habitat_obj[[s]]$F_habitat_weights
    V <- get_variables(t, y, F_wts, xds_obj)
    xds_obj$ML_interface$search_weights[[s]] <- F_wts(t, V) 
    
    F_bad <- xds_obj$ML_interface$habitat_obj[[s]]$F_bad_habitat
    V <- get_variables(t, y, F_bad, xds_obj)
    xds_obj$ML_interface$Qbad <- F_bad(t, V) 
  }
  return(xds_obj)    
}

#' @title Set up no habitats
#' 
#' @description Setup a model for no time spent searching 
#' and no exposure while searching
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a named list to configure `F_habitat_weights`
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
setup_F_habitat_weights = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_habitat_weights", mod_name) 
}

#' @title Setup Bad Habitat Availability 
#' 
#' @description Setup a the function model for no time spent searching 
#' and no exposure while searching
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a list of setup options (see [make_ts_function])
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
setup_F_habitat_weights.static = function(mod_name, xds_obj, s, options=list()){
  F_habitat_weights <- Zero_tV
  class(F_habitat_weights) = "na" 
  xds_obj$ML_interface$habitat_obj[[s]]$F_habitat_weights <- F_habitat_weights
  return(xds_obj) 
}

#' @title Setup Bad Habitat Availability 
#' 
#' @description Setup a the function model for no time spent searching 
#' and no exposure while searching
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a list of setup options (see [make_ts_function])
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_habitat_weights.ts_func = function(mod_name, xds_obj, s, options=list()){
  class(xds_obj$ML_interface$habitat_obj) <- 'dynamic'
  class(xds_obj$ML_interface) <- 'dynamic'
  class(xds_obj$beta) <- 'dynamic'
  F_wts <- make_ts_function(options, N=xds_obj$nHabitats) 
  F_wts -> xds_obj$ML_interface$habitat_obj[[s]]$F_habitat_weights 
  return(xds_obj) 
}

#' @title Set up no habitats
#' 
#' @description Setup a model for no time spent searching 
#' and no exposure while searching
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a named list to configure `F_bad_habitat`
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_bad_habitat = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_bad_habitat", mod_name) 
}

#' @title Setup Bad Habitat Availability 
#' 
#' @description Setup a the function model for no time spent searching 
#' and no exposure while searching
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a list of setup options (see [make_ts_function])
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_bad_habitat.static = function(mod_name, xds_obj, s, options=list()){
  F_bad_habitats <- Zero_tV
  class(F_bad_habitats) = "na" 
  xds_obj$ML_interface$habitat_obj[[s]]$F_bad_habitats <- F_bad_habitats
  return(xds_obj) 
}

#' @title Set up no habitats
#' 
#' @description Setup a model for no time spent searching 
#' and no exposure while searching
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a list of setup options for [make_ts_function]
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_F_bad_habitat.ts_func = function(mod_name, xds_obj, s, options=list()){
  class(xds_obj$ML_interface$habitat_obj) <- 'dynamic'
  class(xds_obj$ML_interface) <- 'dynamic'
  class(xds_obj$beta) <- 'dynamic'
  F_wts <- make_ts_function(options, N=xds_obj$nHabitats) 
  F_wts -> xds_obj$ML_interface$habitat_obj[[s]]$F_bad_habitat 
  return(xds_obj) 
}
