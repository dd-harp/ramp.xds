
#' @title Setup the Travel Object 
#' 
#' @description Setup the object to handle 
#' travel 
#' 
#' + `F_travel` should have the form \eqn{F(t,V)} where \eqn{V}
#' is returned by [get_variables] dispatching on `class(F_travel)`
#' 
#' + `F_travel_eir` should have the form \eqn{F(t,V)} where \eqn{V}
#' is returned by [get_variables] dispatching on `class(F_travel)`
#' 
#' 
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** model object
#' @export
setup_travel_object = function(xds_obj){
  
  xds_obj$XY_interface$travel_obj <- list()  
  class(xds_obj$XY_interface$travel_obj) <- "setup" 
  xds_obj$XY_interface$travel_obj[[1]] <- list() 
  
  xds_obj <- setup_F_travel("static", xds_obj, 1)
  xds_obj <- setup_F_travel_eir("static", xds_obj, 1)
  
  xds_obj$XY_interface$time_at_home = list()  
  xds_obj$XY_interface$time_at_home[[1]] = rep(1, xds_obj$nStrata) 
  
  xds_obj$terms$travel_EIR <- list() 
  xds_obj$terms$travel_EIR[[1]] <- rep(0, xds_obj$nStrata) 
  
  return(xds_obj)
}

#' @title Change the travel EIR 
#' 
#' @description Set a new *static* value for travel EIR
#' 
#' @param time_at_home time spent in spatial domain (*i.e.* in the patches) 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'  
#' @return an `xds` object
#' @export
change_time_at_home = function(time_at_home, xds_obj,  i){
  stopifnot(length(time_at_home) == xds_obj$nStrata[[i]])
  xds_obj$XY_interface$time_at_home[[i]] = time_at_home 
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

#' @title Change the travel EIR 
#' 
#' @description Set a new *static* value for travel EIR
#' 
#' @param teir availability of other blood hosts 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'  
#' @return an `xds` object
#' @export
change_travel_EIR = function(teir, xds_obj,  i){
  stopifnot(length(teir) == xds_obj$nStrata[[i]])
  xds_obj$terms$travel_EIR[[i]] = teir 
  return(xds_obj)
}


#' @title Time Spent F_travel
#' @description This function sets the value of a parameter
#' describing time spent F_travel. It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' 
#' @param t current simulation time
#' @param y variables 
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @export
Travel <- function(t, y, xds_obj) {
  UseMethod("Travel", xds_obj$XY_interface$travel_obj)
}

#' @title Time Spent F_travel
#' @description For a static model for time spent F_travel, the function
#' does not update anything.
#' @inheritParams Travel 
#' @return an **`xds`** object
#' @export
Travel.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Time Spent F_travel
#' @description This function sets a static value for the parameter
#' describing time spent F_travel, resets the class to `static` and
#' then triggers an update to the blood feeding model.
#' It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams Travel 
#' @return an **`xds`** object
#' @export
Travel.setup <- function(t, y, xds_obj) {
  class(xds_obj$XY_interface$travel_obj) <- "static"
  xds_obj$XY_interface <- trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

#' @title Time Spent F_travel
#' @description This function sets the value of a parameter
#' describing time spent F_travel. It is computed in [compute_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams Travel 
#' @return an **`xds`** object
#' @export
Travel.dynamic <- function(t, y, xds_obj) {
  return(travel_dynamics(t, y, xds_obj))
}

#' @title Time Spent Here 
#' 
#' @description This function sets the value of a term that 
#' is the time spent *here,* stored as `time_at_home` 
#' on the `XY_interface.` Here is defined as time 
#' spent in one of the patches in this model, so 
#' travle is time spent elsewhere.  
#' 
#' A function `F_travel` computes the fraction of time spent traveling 
#' stores its complement as `time_at_home.` 
#' 
#' @note The quantity `time_at_home` is part of the `XY_interface`
#' It is used to compute the Time at Risk matrix, and it is also 
#' used to weight local exposure *vs.*  traveling exposure.  
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** model object
#' 
#' @return an **`xds`** object
#' @export
travel_dynamics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nHostSpecies){
    
    F_travel <- xds_obj$XY_interface$travel_obj[[s]]$F_travel
    V <- get_variables(t, y, F_travel, xds_obj)
    xds_obj$XY_interface$time_at_home[[s]] <- 1-F_travel(t,V)
    
    F_travel_eir <- xds_obj$XY_interface$travel_obj[[s]]$F_travel_eir
    V <- get_variables(t, y, F_travel_eir, xds_obj)
    xds_obj$terms$travel_EIR[[s]] <- F_travel_eir(t,V) 
    
  }
  return(xds_obj)    
}

#' @title Set up no travel
#' 
#' @description Setup a model for no time spent traveling 
#' and no exposure while traveling
#' 
#' @param setup_name a string to dispatch `setup_F_travel` 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options setup options for `F_travel` 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_travel = function(setup_name, xds_obj, i, options=list()){
  class(setup_name) = setup_name
  UseMethod("setup_F_travel", setup_name) 
}

#' @title Set up no travel
#' 
#' @description Setup a model for no time spent traveling 
#' and no exposure while traveling
#' 
#' @inheritParams setup_F_travel
#' 
#' @return an **`xds`** model object
#' @export
setup_F_travel.static = function(setup_name, xds_obj, i, options=list()){
  F_travel <- Zero_tV 
  class(F_travel) = "na" 
  xds_obj$XY_interface$travel_obj[[i]]$F_travel = F_travel 
  return(xds_obj) 
}

#' @title Set up no travel
#' 
#' @description Setup a model for no time spent traveling 
#' and no exposure while traveling
#' 
#' @inheritParams setup_F_travel
#' 
#' @return an **`xds`** model object
#' @export
setup_F_travel.ts_func = function(setup_name, xds_obj, i, options=list()){
  class(xds_obj$XY_interface$travel_obj) = 'dynamic'
  class(xds_obj$XY_interface) = 'dynamic'
  class(xds_obj$beta) = 'dynamic'
  xds_obj$XY_interface$travel_obj[[i]]$F_travel= make_ts_function(options) 
  return(xds_obj) 
}

#' @title Setup the Travel EIR 
#' 
#' @description Setup a model for the dEIR 
#' experienced while traveling 
#' 
#' @param setup_name a string to dispatch `setup_F_travel_eir` 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options a named list to set up `F_travel_eir` 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_travel_eir = function(setup_name, xds_obj, i, options){
  class(setup_name) = setup_name
  UseMethod("setup_F_travel_eir", setup_name) 
}

#' @title Set up no travel
#' 
#' @description Setup a model for no time spent traveling 
#' and no exposure while traveling
#' 
#' @inheritParams setup_F_travel_eir
#' 
#' @return an **`xds`** model object
#' @export
setup_F_travel_eir.static = function(setup_name, xds_obj, i, options){
  F_travel_eir <- Zero_tV 
  class(F_travel_eir) = "na" 
  xds_obj$XY_interface$travel_obj[[i]]$F_travel_eir = F_travel_eir
  return(xds_obj) 
}

#' @title Setup the Travel EIR 
#' 
#' @description Setup a model for no time spent traveling 
#' and no exposure while traveling
#' 
#' @param setup_name a 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options options list to set up `F_travel_eir`
#' 
#' @return an **`xds`** model object
#' @export
setup_F_travel_eir.ts_func = function(setup_name, xds_obj, i, options){
  xds_obj$XY_interface$travel_obj[[i]]$F_travel_eir = make_ts_function(options) 
  return(xds_obj) 
}
