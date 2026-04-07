#' @title time spent 
#' 
#' @description
#' The risk of exposure to mosquito-borne pathogens
#' is related to time spent in places where vectors are
#' blood feeding. Each human / host population resides
#' in a patch. Let \eqn{N_p} denote the number of patches
#' and \eqn{N_h} the number of strata. 
#' The time spent matrix, \eqn{\Theta}, is an \eqn{N_p \times N_h} matrix:
#' each columns describes the fraction of time spent by a single
#' population stratum in each patch. 
#' It is expected that most 
#' time is spent in the patch where
#' the stratum resides.  
#' 
#' In **`ramp.xds`,** the time spent matrix is static. In **`xds.forcing`**, 
#' time spent can have a daily pattern, and time at risk weights 
#' time spent by a function describing mosquito daily activity rates  
#' (see [xds_info_time_at_risk]).
#'  
#' @name xds_info_time_spent 
NULL

#' @title Time at Risk
#' 
#' @description
#' Mosquitoes have a daily activity pattern. 
#' If people spend time in different locations at different 
#' times of the day, biting risk in a location is weighted by the 
#' mosquitoes relative activity patterns while there. 
#' 
#'  
#' @name xds_info_time_at_risk
NULL

#' @title Update the time spent Matrix
#' @description Port function for the time spent matrix, \eqn{\Theta}.
#' Dispatches on `class(xds_obj$XY_interface$timespent_obj[[i]])`.
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent <- function(xds_obj, i) {
  UseMethod("update_timespent", xds_obj$XH_obj[[i]]$timespent_obj)
}

#' @title Update the time spent Matrix (static)
#' @description Returns `xds_obj` unmodified; the time spent matrix is static.
#' @inheritParams update_timespent
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent.static <- function(xds_obj, i) { return(xds_obj) }

#' @title Update the time spent Matrix (setup)
#' @description Acknowledges a one-time update to the time spent matrix
#' and sets the port back to `"static"`.
#' @inheritParams update_timespent
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent.setup <- function(xds_obj, i) {
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  class(xds_obj$XH_obj[[i]]$timespent_obj) <- "static"
  return(xds_obj)
}

#' @title Set up (or change) a time spent matrix
#'
#' @param timespent a time spent matrix
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_timespent_matrix = function(timespent, xds_obj, i=1){
  residency_matrix <- get_residence_matrix(xds_obj, 1)
  stopifnot(dim(timespent) == dim(residency_matrix))
  xds_obj$XH_obj[[i]]$timespent <- timespent
  xds_obj$XH_obj[[i]]$timespent_obj = trigger_setup(xds_obj$XH_obj[[i]]$timespent_obj)
  xds_obj$XY_interface = trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}


#' @title Make a time spent matrix, called timespent
#'
#' @param name a matrix or setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_timespent = function(name, xds_obj, i, options = list()){
  if(is.matrix(name)) class(options) <- "as_matrix"
  if(is.character(name)) class(options) <- name
  UseMethod("setup_timespent", options)
}

#' @title Setup no time spent matrix 
#' @description Don't change anything 
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.no_setup = function(name, xds_obj, i, options = list()){
  return(xds_obj)
}

#' @title Make a time spent matrix, called timespent with a here / away
#' @description Implements [setup_timespent] for as_matrix
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.athome = function(name, xds_obj, i, options = list()){
  residence = xds_obj$residence[[i]]
  timespent = make_timespent_athome(xds_obj$nPatches, residence, options)
  change_timespent_matrix(timespent, xds_obj, i)
}

#' @title Make a time spent matrix, called timespent
#' @param nPatches is the number of patches
#' @param residence is the home patch for each stratum
#' @param atHome is the fraction of time spent at home
#' @param options is a set of options that overwrites the defaults
#' @param travel is the fraction of time spent traveling
#' @return a [matrix]
#' @export
make_timespent_athome = function(nPatches, residence, options=list(), atHome=1, travel=0) {with(options,{
  nStrata = length(residence)
  away = ifelse(nPatches == 1, 0, (1-atHome-travel)/(nPatches-1))
  atHome = ifelse(nPatches == 1, 1-travel, atHome)
  timespent <- matrix(away, nPatches, length(residence))
  timespent[cbind(residence, c(1:nStrata))] <- atHome
  return(timespent)
})}

#' @title Pass a pre-configured timespent
#' @description Implements [setup_timespent] for as_matrix
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.as_matrix = function(name, xds_obj, i, options=list()){
  timespent = name
  change_timespent_matrix(timespent, xds_obj, i)
}

#' @title Develop a time spent matrix from a kernel and xy-coordinates
#' @description Implements [setup_timespent] for kernels
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.xy = function(name, xds_obj, i, options=list()) {
  residence = xds_obj$B_interface[[i]]$residence
  timespent = with(options, make_timespent_xy(xy, residence, kern, stay, travel))
  change_timespent_matrix(timespent, xds_obj, i)
}

#' @title Make a time spent matrix, called timespent
#' @param xy is the xy-locations of the patches
#' @param residence is the home patch for each stratum
#' @param kern is a function that gives weight by distance
#' @param stay is the fraction of time spent at home
#' @param travel is the fraction of time spent traveling
#' @return a [matrix]
#' @export
make_timespent_xy = function(xy, residence, kern, stay, travel) {
  nPatches = dim(xy)[1]
  nStrata = length(residence)
  stopifnot(length(stay)==nStrata)
  stopifnot(length(travel)==nStrata)
  timespent = matrix(0, nPatches, nStrata)
  for(i in 1:nStrata){
    j = residence[i]
    dd = sqrt((xy[j,1] - xy[,1])^2 + (xy[j,2] - xy[,2])^2)
    wts = kern(dd)
    wts[j] = 0
    wts = (1-stay[i]-travel[i])*wts/sum(wts[-j])
    wts[j] = stay[i]
    timespent[,i] = wts
  }
  return(timespent)
}

#' @title time spent
#' @description Compute and store host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi},
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @return an **`xds`** object
#' @keywords internal
#' @export
timespent = function(t, y, xds_obj){
  UseMethod("timespent", xds_obj$XY_interface)
}

#' @title Compute time spent objects: setup for static models
#' @description This sets up host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
#' for static models.
#' @details The mixing matrix, \eqn{\beta}, depends on
#' time spent terms, so the class of `xds_obj$beta` must also
#' be updated, if they are not dynamic, so [trigger_setup] is called.
#' @inheritParams timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
timespent.setup = function(t, y, xds_obj){
  class(xds_obj$XY_interface) <- 'static'
  xds_obj$beta <- trigger_setup(xds_obj$beta)
  xds_obj <- blood_feeding_dynamics(t, y, xds_obj)
  return(return(xds_obj))
}

#' @title Compute time spent objects: static models
#' @description Return the time spent objects unmodified
#' @inheritParams timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
timespent.static = function(t, y, xds_obj){
  return(xds_obj)
}


