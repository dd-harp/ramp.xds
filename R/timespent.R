

#' @title Check Time Spent Matrix
#' 
#' @description
#' Check that 
#' + there ar \eqn{N_p} columns and \eqn{N_h} rows
#' + the column sums are at most 1`
#' 
#' @param TS a time spent matrix
#' @param Np the number of patches
#' @param Nh the number of population strata
#' 
#' @seealso [xds_info_time_spent]
#' @export
check_timespent_matrix = function(TS, Np, Nh){
  stopifnot(is.matrix(TS))
  stopifnot(TS>=0)
  stopifnot(dim(TS)==c(Np, Nh))
  cs <- colSums(TS)
  stopifnot(cs <= 1)
}

#' @title Change the time spent matrix
#' @description
#' Update the time spent matrix for the \eqn{i^{th}} host species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param timespent a time spent matrix
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_timespent_matrix = function(timespent, xds_obj, i=1){
  check_timespent_matrix(timespent, xds_obj$nPatches, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$timespent <- timespent
  xds_obj$XH_obj[[i]]$timespent_obj <- make_static_obj()
  return(xds_obj)
}

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



#' @title Set up a time spent matrix
#'
#' @param name a matrix or setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_timespent = function(name, xds_obj, options = list(), i=1){
  if(is.matrix(name)) class(options) = "as_matrix"
  if(is.character(name)) class(options) = name
  UseMethod("setup_timespent", options)
}



#' @title Set up a time spent matrix
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `TSname = name$name` 
#' + `options = name`
#' and call `setup_timespent(TSname, xds_obj, options, s)` 
#'
#' @inheritParams setup_timespent
#'
#' @return a [matrix]
#' @keywords internal
#' @export
setup_timespent.list = function(name, xds_obj, options=list(), i=1){
  options = name
  TSname = name$name
  if(is.null(TSname)) TSname = "no_setup"
  xds_obj <- setup_timespent(TSname, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up no time spent matrix 
#' @description Don't change anything 
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.setup = function(name, xds_obj, options = list(), i=1){
  stopifnot(with(options, exists("residence")))
  xds_obj$XH_obj[[i]]$residence = options$residence
  res_mat <- get_residence_matrix(xds_obj, i)
  xds_obj <- change_timespent_matrix(res_mat, xds_obj, i)
  return(xds_obj)
}


#' @title Set up no time spent matrix 
#' @description Don't change anything 
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up a time spent matrix
#' @description Implements the "at_home" case for [setup_timespent]. See [make_timespent_at_home]
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.at_home = function(name, xds_obj, options = list(), i=1){
  residence = xds_obj$residence[[i]]
  timespent = make_timespent_at_home(xds_obj$nPatches, residence, options)
  xds_obj <- change_timespent_matrix(timespent, xds_obj, i)
  return(xds_obj)
}

#' @title Make a time spent matrix 
#' @description 
#' A fraction of time is spent at home. The remaining time 
#' is divided equally among the other patches. 
#' @param nPatches is the number of patches
#' @param residence is the home patch for each stratum
#' @param options is a set of options that overwrites the defaults
#' @param at_home is the fraction of time spent at home
#' @param not_at_risk is the fraction of time not at risk
#' @return a [matrix]
#' @export
make_timespent_at_home = function(nPatches, residence, options=list(), at_home=1, not_at_risk=0) {with(options,{
  nStrata = length(residence)
  away = ifelse(nPatches == 1, 0, (1-at_home-not_at_risk)/(nPatches-1))
  at_home = ifelse(nPatches == 1, 1-not_at_risk, at_home)
  timespent <- matrix(away, nPatches, length(residence))
  timespent[cbind(residence, c(1:nStrata))] <- at_home
  return(timespent)
})}

#' @title Set up a time spent matrix
#' @description 
#' Pass a pre-configured time spent matrix. If it passes 
#' the checks, it replaces the current timespent matrix.
#' 
#' If called with `name = "as_matrix"`, the time spent
#' matrix must be at `options$timespent_matrix` 
#' 
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.as_matrix = function(name, xds_obj, options=list(), i=1){

  if(is.list(options)) 
    TS_matrix = options$timespent_matrix
  if(is.matrix(name))
    TS_matrix = name

  xds_obj <- change_timespent_matrix(TS_matrix, xds_obj, i)
  
  return(xds_obj)
}

#' @title Set up a time spent matrix
#' @description Implements [setup_timespent] from a set of xy coordinates
#' and a spatial kernel. See [make_timespent_xy]
#' @inheritParams setup_timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_timespent.xy = function(name, xds_obj, options=list(), i=1) {
  residence = get_residence(xds_obj, i)
  timespent = with(options, make_timespent_xy(xds_obj, xy, residence, stay, kern, options))
  xds_obj <- change_timespent_matrix(timespent, xds_obj, i)
  return(xds_obj)
}

#' @title Make a time spent matrix using location data
#' @param xds_obj the **`xds`** model object
#' @param xy is the xy-locations of the patches
#' @param residence is the home patch for each stratum
#' @param stay is the fraction of time spent at home
#' @param kern is a function to compute time spent away from home
#' @param kopts options to pass to the kernel
#' @return a [matrix]
#' @export
make_timespent_xy = function(xds_obj, xy, residence, stay, kern, kopts=list()) {
  nPatches = dim(xy)[1]
  nStrata = length(residence)
  stopifnot(length(stay)==nStrata)
  timespent = matrix(0, nPatches, nStrata)
  for(i in 1:nStrata){
    j = residence[i]
    dd = sqrt((xy[j,1] - xy[,1])^2 + (xy[j,2] - xy[,2])^2)
    wts = kern(dd, xds_obj, kopts)
    wts[j] = 0
    wts = (1-stay[i])*wts/sum(wts[-j])
    wts[j] = stay[i]
    timespent[,i] = wts
  }
  return(timespent)
}

#' @title Update the time spent Matrix
#' @description Port function for the time spent matrix, \eqn{\Theta}.
#' Dispatches on `class(xds_obj$XY_interface$timespent_obj[[i]])`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent <- function(t, y, xds_obj, i) {
  UseMethod("update_timespent", xds_obj$XH_obj[[i]]$timespent_obj)
}

#' @title Update timespent 
#' 
#' @description A utility to ensure that the timespent
#' is updated properly after a change
#' 
#' @inheritParams update_timespent
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent.static <- function(t, y, xds_obj, i) {return(xds_obj)}


#' @title Update timespent
#' 
#' @description Update timespent when it changes dynamically
#' 
#' @inheritParams update_timespent
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent.dynamic <- function(t, y, xds_obj, i) { 
  timespent(t, y, xds_obj, i)
}

#' @title time spent
#'
#' @description Compute and store a 
#' time spent matrix 
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
timespent = function(t, y, xds_obj, i){
  UseMethod("timespent", xds_obj$XH_obj[[i]]$timespent_obj)
}

#' @title Compute time spent
#' @description Return the time spent objects unmodified
#' @inheritParams timespent
#' @return an **`xds`** object
#' @keywords internal
#' @export
timespent.static = function(t, y, xds_obj, i){
  return(xds_obj)
}

#' @title Get the Time Spent Matrix
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [matrix]
#' @export
get_timespent_matrix = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$timespent)
}

