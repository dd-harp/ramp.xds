
#' @title Setup the Blood Hosts Object 
#' 
#' @description 
#' 
#' Setup the object to handle 
#' other blood_hosts -- potential blood hosts
#' that are not hosts for the pathogen  
#' 
#' @param xds_obj an **`xds`** model object 
#' 
#' @return an **`xds`** model object
#' @export
setup_blood_host_object = function(xds_obj){
  blood <- list()
  Fbl <- Zero_tV
  class(Fbl) <- "na"
  blood$F_blood_host<- Fbl 
  xds_obj$XY_interface$blood_host_obj <- list() 
  xds_obj$XY_interface$blood_host_obj[[1]] <- blood
  class(xds_obj$XY_interface$blood_host_obj) <- "setup" 
  xds_obj$XY_interface$blood_hosts = list()
  xds_obj$XY_interface$blood_hosts[[1]] = rep(0, xds_obj$nPatches) 
  return(xds_obj)
}

#' @title Set static blood feeding search weights
#' @description Set the blood feeding search weights, \eqn{\omega}, for a set of host strata
#' @param blood_hosts availability of other blood hosts 
#' @param xds_obj an `xds` object
#' @param s the vector species index
#' @return an `xds` object
#' @export
change_blood_hosts = function(blood_hosts, xds_obj,  s){
  stopifnot(length(blood_hosts) == xds_obj$nPatches)
  xds_obj$XY_interface$blood_hosts[[s]] = blood_hosts 
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}


#' @title Other Blood Hosts 
#' 
#' @description This function sets the value of a term 
#' describing the availability of ovi traps
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** object
#'
#' @return an **`xds`** object
#' @export
BloodHosts <- function(t, y, xds_obj) {
  UseMethod("BloodHosts", xds_obj$XY_interface$blood_host_obj)
}

#' @title Time Spent F_blood_hosts
#' @description For a static model for time spent F_blood_hosts, the function
#' does not update anything.
#' @inheritParams BloodHosts 
#' @return an **`xds`** object
#' @export
BloodHosts.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Time Spent F_blood_hosts
#' @description This function sets a static value for the parameter
#' describing time spent F_blood_hosts, resets the class to `static` and
#' then triggers an update to the blood feeding model.
#' It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams BloodHosts 
#' @return an **`xds`** object
#' @export
BloodHosts.setup <- function(t, y, xds_obj) {
  class(xds_obj$XY_interface$blood_host_obj) <- "static"
  xds_obj$XY_interface <- trigger_setup(xds_obj$XY_interface)
  xds_obj <- blood_hosts_dynamics(t, y, xds_obj)
  return(xds_obj)
}

#' @title Time Spent F_blood_hosts
#' @description This function sets the value of a parameter
#' describing time spent F_blood_hosts. It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams BloodHosts 
#' @return an **`xds`** object
#' @export
BloodHosts.dynamic <- function(t, y, xds_obj) {
  return(blood_hosts_dynamics(t, y, xds_obj)) 
}

#' @title blood_hosts Dynamics 
#' 
#' @description 
#'  
#' on the `XY_interface.` Here is defined as time 
#' spent in one of the patches in this model, so 
#' travle is time spent elsewhere.  
#' 
#' A function `F_blood_hosts` computes `TiSp_frac_blood_hostsing` and  
#' this function sets `TiSp_frac_here.` 
#' 
#' @note The quantity `TiSp_frac_here` is part of the `XY_interface`
#' It is used to compute the Time at Risk matrix, and it is also 
#' used to weight local exposure *vs.*  blood_hostsing exposure.  
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
blood_hosts_dynamics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nVectorSpecies){
    F_blood <- xds_obj$XY_interface$blood_host_obj[[s]]$F_blood_host
    V <- get_variables(t, y, F_blood, xds_obj)
    xds_obj$XY_interface$blood_hosts[[s]] <- F_blood(t, V)  
  }
  return(xds_obj)    
}

#' @title Set up no blood_hosts
#' 
#' @description Setup a model for no time spent blood_hostsing 
#' and no exposure while blood_hostsing
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index
#' @param options a list to configure `F_blood_hosts`
#' 
#' @return an **`xds`** model object
#' @export
setup_blood_hosts = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_blood_hosts", mod_name) 
}

#' @title Set up no blood_hosts
#' 
#' @description Setup a model for no time spent blood_hostsing 
#' and no exposure while blood_hostsing
#' 
#' @inheritParams setup_blood_hosts 
#' 
#' @return an **`xds`** model object
#' @export
setup_blood_hosts.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$XY_interface$blood_host_obj) <- 'dynamic'
  class(xds_obj$XY_interface) <- 'dynamic'
  class(xds_obj$terms$beta) <- 'dynamic'
  FF <- make_ts_function(options) 
  FF -> xds_obj$XY_interface$blood_host_obj[[s]]$F_blood_hosts  
  return(xds_obj) 
}

