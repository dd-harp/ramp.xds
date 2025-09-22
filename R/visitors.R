
#' @title Setup the Visitors Object
#' 
#' @description Setup a an object
#' to model blood feeding and importation  
#' from visitors 
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' @export
setup_visitor_object = function(xds_obj){
  vis <- list()
  class(vis) <- "setup"
  FtV <- Zero_tV
  class(FtV) <- "na"
  vis$F_visitors <- FtV
  vis$F_vis_kappa <- FtV 
  xds_obj$XY_interface$visitor_obj <- list() 
  class(xds_obj$XY_interface$visitor_obj) <- "setup" 
  xds_obj$XY_interface$visitor_obj[[1]] <- vis
  xds_obj$XY_interface$visitors = list()
  xds_obj$XY_interface$visitors[[1]] = rep(0, xds_obj$nPatches) 
  xds_obj$XY_interface$vis_kappa = list()
  xds_obj$XY_interface$vis_kappa[[1]] = rep(0, xds_obj$nPatches) 
  return(xds_obj)
}

#' @title Visitors 
#' @description This function sets the value of a parameter
#' describing time spent F_visitors. It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @export
Visitors <- function(t, y, xds_obj) {
  UseMethod("Visitors", xds_obj$XY_interface$visitor_obj)
}

#' @title Visitors 
#' @description For a static model for time spent F_visitors, the function
#' does not update anything.
#' @inheritParams Visitors 
#' @return an **`xds`** object
#' @export
Visitors.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Visitors 
#' @description This function sets a static value for the parameter
#' describing time spent F_visitors, resets the class to `static` and
#' then triggers an update to the blood feeding model.
#' It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams Visitors 
#' @return an **`xds`** object
#' @export
Visitors.setup <- function(t, y, xds_obj) {
  class(xds_obj$XY_interface$visitor_obj) <- "static"
  class(xds_obj$XY_interface) <- "setup" 
  xds_obj <- visitor_dynamics(t, y, xds_obj)
  return(xds_obj)
}

#' @title Visitor Dynamics 
#' 
#' @description This function sets the value of a parameter
#' describing the availability of visitors. 
#'
#' @inheritParams Visitors 
#' 
#' @return an **`xds`** object
#' @export
Visitors.dynamic <- function(t, y, xds_obj) {
  xds_obj <- visitor_dynamics(t, y, xds_obj)
  return(xds_obj)
}

#' @title Availabilit of Visitors 
#' 
#' @description This function sets the value of a term that 
#' is the time spent *here,* stored as `TiSp_frac_visitorsing` 
#' on the `XY_interface.` Here is defined as time 
#' spent in one of the patches in this model, so 
#' travle is time spent elsewhere.  
#' 
#' A function `F_visitors` computes `TiSp_frac_visitorsing` and  
#' this function sets `TiSp_frac_here.` 
#' 
#' @note The quantity `TiSp_frac_here` is part of the `XY_interface`
#' It is used to compute the Time at Risk matrix, and it is also 
#' used to weight local exposure *vs.*  visitorsing exposure.  
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** model object
#'
#' 
#' @return an **`xds`** object
#' @export
visitor_dynamics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nHostSpecies){

    F_vis <- xds_obj$XY_interface$visitor_obj[[s]]$F_visitors
    V <- get_variables(t, y, F_vis, xds_obj)
    xds_obj$XY_interface$visitors[[s]] <- F_vis(t, V) 
    
    F_vkap <- xds_obj$XY_interface$visitor_obj[[s]]$F_vis_kappa
    V <- get_variables(t, y, F_vkap, xds_obj)
    xds_obj$XY_interface$vis_kappa[[s]] <- F_vkap(t, V) 
  }
  return(xds_obj)    
}

#' @title Set up no visitors
#' 
#' @description Setup a model for no time spent visitorsing 
#' and no exposure while visitorsing
#' 
#' @param mod_name the model name 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a named list to setup `F_visitors`
#' 
#' @return an **`xds`** model object
#' @export
setup_F_visitors = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_visitors", mod_name) 
}

#' @title Set up no visitors
#' 
#' @description Setup a model for no time spent visitorsing 
#' and no exposure while visitorsing
#' 
#' @inheritParams setup_F_visitors
#' 
#' @return an **`xds`** model object
#' @export
setup_F_visitors.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$XY_interface$visitor_obj) <- 'dynamic'
  class(xds_obj$XY_interface) <- 'dynamic'
  class(xds_obj$beta) <- 'dynamic'
  Fv <- make_ts_function(options)
  Fv -> xds_obj$XY_interface$visitor_obj[[s]]$F_visitors 
  return(xds_obj) 
}

#' @title Set up no visitors
#' 
#' @description Setup a model for no time spent visitorsing 
#' and no exposure while visitorsing
#' 
#' @param mod_name the model name 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a list to configure `F_vis_kappa` 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_vis_kappa = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_visitors_kappa", mod_name) 
}

#' @title Set  
#' 
#' @description Setup a model for the net infectiousness 
#' of visitors
#' 
#' @inheritParams setup_F_vis_kappa
#' 
#' @return an **`xds`** model object
#' @export
setup_F_vis_kappa.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$XY_interface$visitor_obj) <- 'dynamic'
  class(xds_obj$XY_interface) <- 'dynamic'
  class(xds_obj$beta) <- 'dynamic'
  Fv <- make_ts_function(options)
  Fv -> xds_obj$XY_interface$visitor_obj[[s]]$F_vis_kappa
  return(xds_obj) 
}
