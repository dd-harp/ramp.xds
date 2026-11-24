# X

#' @title Time Away
#' 
#' @description
#' 
#' \describe{
#'   \item{\eqn{\delta}}{time away}
#' } 
#'  
#' Time away is defined by travel, or time spent 
#' outside the patches. Time away
#' is used to model travel malaria in [Exposure], and it is used 
#' to compute host availability. 
#' 
#' **Travel Malaria** 
#' 
#' The risk of malaria while traveling is computed in [Exposure] as 
#' the product of time away and the travel EIR \eqn{(E_\delta)}. Daily travel
#' risk is \deqn{\delta E_\delta}
#' 
#' **Host Availability**
#'  
#' If human population density is \eqn{H}, but since a fraction \eqn{\delta} is traveling,
#'  the expected density of humans at any point in time is \deqn{(1-\delta)H.}
#'
#' @seealso [setup_time_away], [human_availability]
#' @name time_away
#' @aliases xds_info_time_away
NULL

#' @title Check time away
#' 
#' @description
#' Check that 
#' + the input is numeric
#' + all elements are non-negative
#' + all elements are at most 1
#' + the length is `Nh`
#' 
#' @param TA a time away vector
#' @param Nh the number of population strata
#' 
#' @seealso [xds_info_time_spent]
#' @export
check_time_away = function(TA, Nh){
  stopifnot(is.numeric(TA))
  stopifnot(TA>=0)
  stopifnot(TA<=1)
  stopifnot(length(TA)==Nh) 
}

#' @title Change time away
#' @description
#' Update time away for the \eqn{i^{th}} host species, and
#' trigger updates for the `XY` interface. 
#' 
#' @param time_away a time away vector
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** model object
#' @export
change_time_away = function(time_away, xds_obj, i=1){
  check_time_away(time_away, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$time_away <- time_away
  xds_obj$XH_obj[[i]]$time_away_obj <- make_static_obj()
  xds_obj$XH_obj[[i]]$time_away_obj$time_away <- time_away
  xds_obj <- update_XY_interface(xds_obj)
  return(xds_obj)
}

#' @title Set up time away
#'
#' @param name a setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** model object
#' @seealso [time_away]
#'
#' @export
setup_time_away = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_time_away", options)
}

#' @title Set up time away
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `TAname = name$name` 
#' + `options = name`
#' and call `setup_time_away(TAname, xds_obj, options, i)`
#'
#' @inheritParams setup_time_away
#'
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_time_away.list = function(name, xds_obj, options=list(), i=1){
  options = name
  TAname = name$name
  if(is.null(TAname)) TAname = "no_setup"
  xds_obj <- setup_time_away(TAname, xds_obj, options, i)
  return(xds_obj)
}

#' @title Leave time away unchanged
#' @description Don't change anything 
#' @inheritParams setup_time_away
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_time_away.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up default time away
#' @description Initialize time away to zero for each stratum of the selected
#' host species and initialize a static port.
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_time_away.setup = function(name, xds_obj, options=list(), i=1){
  time_away <- rep(0, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$time_away <- time_away
  xds_obj$XH_obj[[i]]$time_away_obj <- make_static_obj()
  return(xds_obj)
}

#' @title Set up no travel
#' @description Set time away to zero for each stratum of the selected host
#' species using [change_time_away].
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_time_away.no_travel = function(name, xds_obj, options=list(), i=1){
  TA <- rep(0, xds_obj$nStrata[i])
  xds_obj <- change_time_away(TA, xds_obj, i)
  return(xds_obj)
}

#' @title Set up time away
#' @description 
#' Pass a pre-configured time away vector. If it passes
#' the checks, it replaces the current time_away.
#' 
#' If called with `name = "as_is"`, the time away vector
#' must be at `options$time_away`.
#' 
#' @inheritParams setup_time_away
#' @return an **`xds`** model object
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
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_time_away.random = function(name, xds_obj, options = list(), i=1){
  time_away = make_time_away_random(xds_obj$nStrata[i], options)
  xds_obj <- change_time_away(time_away, xds_obj, i)
  return(xds_obj)
}

#' @title Make random time away
#' 
#' @description Set up a random vector 
#' 
#' @param nStrata is the number of population strata
#' @param options is a set of options that overrides the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_time_away_random = function(nStrata, options=list(), f_rand = stats::rbeta, p1=50, p2=1000) {with(options,{
  time_away <- f_rand(nStrata, p1, p2)
  return(time_away)
})}

#' @title Update time away
#' @description Port function for time away.
#' Dispatches on `class(xds_obj$XH_obj[[i]]$time_away_obj)`.
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_time_away <- function(t, y, xds_obj, i) {
  UseMethod("update_time_away", xds_obj$XH_obj[[i]]$time_away_obj)
}

#' @title Update time_away 
#' 
#' @description Return `xds_obj` unchanged for a static time away port.
#' 
#' @inheritParams update_time_away
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_time_away.static <- function(t, y, xds_obj, i) {return(xds_obj)}


#' @title Get time away
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a [numeric] vector of length `nStrata[i]`
#' @export
get_time_away = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$time_away)
}
