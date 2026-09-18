

#' @title Check Time Spent 
#' 
#' @description
#' Check that 
#' + there ar \eqn{N_p} columns and \eqn{N_h} rows
#' + the column sums are at most 1`
#' 
#' @param TA a time spent vector 
#' @param Nh the number of population strata
#' 
#' @seealso [xds_info_time_spent]
#' @export
check_time_away = function(TA, Nh){
  stopifnot(is.numeric(TA))
  stopifnot(TA>=0)
  stopifnot(length(TA)==Nh) 
}

#' @title Change the time spent 
#' @description
#' Update the time spent  for the \eqn{i^{th}} host species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param time_away a time spent vector 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @export
change_time_away = function(time_away, xds_obj, i=1){
  check_time_away(time_away, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$time_away <- time_away
  xds_obj$XH_obj[[i]]$time_away_obj <- make_static_obj()
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- compute_TaR(xds_obj,0)
  xds_obj <- compute_WB(0, y, xds_obj)
  xds_obj <- compute_local_frac(xds_obj)
  xds_obj <- compute_beta(0, y, xds_obj)
  xds_obj <- compute_EIR(0, y, xds_obj)
  xds_obj <- compute_kappa(0, y, xds_obj)
  return(xds_obj)
}

#' @title Set up time spent 
#'
#' @param name a  or setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_time_away = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_time_away", options)
}

#' @title Set up a time spent 
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `TAname = name$name` 
#' + `options = name`
#' and call `setup_time_away(TAname, xds_obj, options, s)` 
#'
#' @inheritParams setup_time_away
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_time_away.list = function(name, xds_obj, options=list(), i=1){
  options = name
  TAname = name$name
  if(is.null(TAname)) TAname = "no_setup"
  xds_obj <- setup_time_away(TAname, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up no time spent  
#' @description Don't change anything 
#' @inheritParams setup_time_away
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_time_away.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up a time spent 
#' @description 
#' Pass a pre-configured time spent . If it passes 
#' the checks, it replaces the current time_away .
#' 
#' If called with `name = "as_is"`, the time spent
#'  must be at `options$time_away` 
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_time_away.setup = function(name, xds_obj, options=list(), i=1){
  time_away <- rep(0, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$time_away <- time_away
  xds_obj$XH_obj[[i]]$time_away_obj <- make_static_obj()
  return(xds_obj)
}

#' @title Set up a time spent 
#' @description 
#' Pass a pre-configured time spent . If it passes 
#' the checks, it replaces the current time_away .
#' 
#' If called with `name = "as_is"`, the time spent
#'  must be at `options$time_away` 
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_time_away.no_travel = function(name, xds_obj, options=list(), i=1){
  TA <- rep(0, xds_obj$nStrata[i])
  xds_obj <- change_time_away(TA, xds_obj, i)
  return(xds_obj)
}

#' @title Set up a time spent 
#' @description 
#' Pass a pre-configured time spent . If it passes 
#' the checks, it replaces the current time_away .
#' 
#' If called with `name = "as_is"`, the time spent
#'  must be at `options$time_away` 
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_time_away.as_is = function(name, xds_obj, options=list(), i=1){
  
  if(is.list(options)) 
    TA = options$time_away
  if(is.numeric(name))
    TA = name
  
  xds_obj <- change_time_away(TA, xds_obj, i)
  
  return(xds_obj)
}

#' @title Set up time away
#' 
#' @description Set up random values for time away
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_time_away.random = function(name, xds_obj, options = list(), i=1){
  time_away = make_time_away_random(xds_obj$nStrata[i], options)
  xds_obj <- change_time_away(time_away, xds_obj, i)
  return(xds_obj)
}

#' @title Make other blood hosts 
#' 
#' @description Set up a random vector 
#' 
#' @param nStrata is the number of population strata
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_time_away_random = function(nStrata, options=list(), f_rand = stats::rbeta, p1=50, p2=1000) {with(options,{
  time_away <- f_rand(nStrata, p1, p2)
  return(time_away)
})}

#' @title Update the time spent 
#' @description Port function for the time spent , \eqn{\Theta}.
#' Dispatches on `class(xds_obj$XY_interface$time_away_obj[[i]])`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_time_away <- function(t, y, xds_obj, i) {
  UseMethod("update_time_away", xds_obj$XH_obj[[i]]$time_away_obj)
}

#' @title Update time_away 
#' 
#' @description A utility to ensure that the time_away
#' is updated properly after a change
#' 
#' @inheritParams update_time_away
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_time_away.static <- function(t, y, xds_obj, i) {return(xds_obj)}


#' @title Get the Time Spent 
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a []
#' @export
get_time_away = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$time_away)
}

