#' @title Time Spent 
#' 
#' @description
#' The risk of exposure to mosquito-borne pathogens
#' is related to time spent in places where vectors are
#' blood feeding. Each human / host population resides
#' in a patch. Let \eqn{N_p} denote the number of patches
#' and \eqn{N_h} the number of strata. 
#' The time spent matrix, $\Theta$, is an \eqn{N_p \times N_h} matrix:
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

#' @title Set up (or change) a Time Spent matrix
#'
#' @param TimeSpent a time spent matrix
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_TimeSpent_matrix = function(TimeSpent, xds_obj, i=1){
  stopifnot(dim(TimeSpent) == dim(xds_obj$XY_interface$residency_matrix[[i]]))
  xds_obj$XY_interface$TimeSpent[[i]] <- TimeSpent
  xds_obj$XY_interface <- trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

#' @title Get the Time Spent Matrix
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
get_TimeSpent_matrix = function(xds_obj, i=1){
  return(xds_obj$XY_interface$TimeSpent[[i]])
}


#' @title Make a time spent matrix, called TimeSpent
#'
#' @param TimeSpent a matrix or a setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_TimeSpent = function(TimeSpent, xds_obj, i, options = list()){
  if(is.matrix(TimeSpent)) class(options) <- "as_matrix"
  if(is.character(TimeSpent)) class(options) <- TimeSpent
  UseMethod("setup_TimeSpent", options)
}

#' @title Make a mosquito dispersal matrix, called TimeSpent with a here / away
#' @description Implements [setup_TimeSpent] for as_matrix
#' @inheritParams setup_TimeSpent
#' @return a [list]
#' @keywords internal
#' @export
setup_TimeSpent.athome = function(TimeSpent, xds_obj, i, options = list()){
  residence = xds_obj$residence[[i]]
  TimeSpent = make_TimeSpent_athome(xds_obj$nPatches, residence, options)
  change_TimeSpent_matrix(TimeSpent, xds_obj, i)
}

#' @title Make a mosquito dispersal matrix, called TimeSpent
#' @param nPatches is the number of patches
#' @param residence is the home patch for each stratum
#' @param atHome is the fraction of time spent at home
#' @param options is a set of options that overwrites the defaults
#' @param travel is the fraction of time spent traveling
#' @return a [matrix]
#' @export
make_TimeSpent_athome = function(nPatches, residence, options=list(), atHome=1, travel=0) {with(options,{
  nStrata = length(residence)
  away = ifelse(nPatches == 1, 0, (1-atHome-travel)/(nPatches-1))
  atHome = ifelse(nPatches == 1, 1-travel, atHome)
  TimeSpent <- matrix(away, nPatches, length(residence))
  TimeSpent[cbind(residence, c(1:nStrata))] <- atHome
  return(TimeSpent)
})}

#' @title Pass a pre-configured TimeSpent
#' @description Implements [setup_TimeSpent] for as_matrix
#' @inheritParams setup_TimeSpent
#' @return a [list]
#' @keywords internal
#' @export
setup_TimeSpent.as_matrix = function(TimeSpent, xds_obj, i, options=list()){
  change_TimeSpent_matrix(TimeSpent, xds_obj, i)
}

#' @title Develop a mosquito dispersal matrix from a kernel and xy-coordinates
#' @description Implements [setup_TimeSpent] for kernels
#' @inheritParams setup_TimeSpent
#' @return a [list]
#' @keywords internal
#' @export
setup_TimeSpent.xy = function(TimeSpent, xds_obj, i, options=list()) {
  residence = xds_obj$B_interface[[i]]$residence
  TimeSpent = with(options, make_TimeSpent_xy(xy, residence, kern, stay, travel))
  change_TimeSpent_matrix(TimeSpent, xds_obj, i)
}

#' @title Make a mosquito dispersal matrix, called TimeSpent
#' @param xy is the xy-locations of the patches
#' @param residence is the home patch for each stratum
#' @param kern is a function that gives weight by distance
#' @param stay is the fraction of time spent at home
#' @param travel is the fraction of time spent traveling
#' @return a [matrix]
#' @export
make_TimeSpent_xy = function(xy, residence, kern, stay, travel) {
  nPatches = dim(xy)[1]
  nStrata = length(residence)
  stopifnot(length(stay)==nStrata)
  stopifnot(length(travel)==nStrata)
  TimeSpent = matrix(0, nPatches, nStrata)
  for(i in 1:nStrata){
    j = residence[i]
    dd = sqrt((xy[j,1] - xy[,1])^2 + (xy[j,2] - xy[,2])^2)
    wts = kern(dd)
    wts[j] = 0
    wts = (1-stay[i]-travel[i])*wts/sum(wts[-j])
    wts[j] = stay[i]
    TimeSpent[,i] = wts
  }
  return(TimeSpent)
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
TimeSpent = function(t, y, xds_obj){
  UseMethod("TimeSpent", xds_obj$XY_interface)
}

#' @title Compute time spent objects: setup for static models
#' @description This sets up host available, \eqn{W},
#' total blood host available, \eqn{B},
#' the time spent matrix \eqn{\Theta}, and the time-at-risk matrix \eqn{\Psi}
#' for static models.
#' @details The mixing matrix, \eqn{\beta}, depends on
#' time spent terms, so the class of `xds_obj$beta` must also
#' be updated, if they are not dynamic, so [trigger_setup] is called.
#' @inheritParams TimeSpent
#' @return an **`xds`** object
#' @keywords internal
#' @export
TimeSpent.setup = function(t, y, xds_obj){
  class(xds_obj$XY_interface) <- 'static'
  xds_obj$beta <- trigger_setup(xds_obj$beta)
  xds_obj <- blood_feeding_dynamics(t, y, xds_obj)
  return(return(xds_obj))
}

#' @title Compute time spent objects: static models
#' @description Return the time spent objects unmodified
#' @inheritParams TimeSpent
#' @return an **`xds`** object
#' @keywords internal
#' @export
TimeSpent.static = function(t, y, xds_obj){
  return(xds_obj)
}


