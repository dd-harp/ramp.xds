#' @title Check other blood hosts
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(blood_traps) == nPatc
#' 
#' 
#' @param blood_traps traps availability
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_blood_traps = function(blood_traps, nP){
  stopifnot(is.numeric(blood_traps))
  stopifnot(blood_traps >= 0)
  stopifnot(length(blood_traps) == nP) 
}

#' @title Change the blood_traps

#' @description
#' Update the blood_traps for the \eqn{s^{th}} vector species, and 
#' trigger updates for the `XY` interface. 
#' 
#' @param blood_traps a blood_traps
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_blood_traps = function(blood_traps, xds_obj, s=1){
  check_blood_traps(blood_traps, xds_obj$nPatches)
  xds_obj$patches$traps[[s]] <- blood_traps
  xds_obj$patches$traps_obj[[s]] <- make_static_obj()
  return(xds_obj)
}

#' @title Set up a blood_traps
#'
#' @param name a or setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the host species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_blood_traps = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_blood_traps", options)
}

#' @title Set up a blood_traps
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `OBHname = name$name` 
#' + `options = name`
#' and call `setup_blood_traps(OBHname, xds_obj, options, s)` 
#'
#' @inheritParams setup_blood_traps
#'
#' @return an **xds** object
#' 
#' @keywords internal
#' @export
setup_blood_traps.list = function(name, xds_obj, options=list(), s=1){
  options = name
  OBHname = name$name
  if(is.null(OBHname)) OBHname = "no_setup"
  xds_obj <- setup_blood_traps(OBHname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no blood_traps 
#' @description Don't change anything 
#' @inheritParams setup_blood_traps
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_traps.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up a blood_traps
#' 
#' @description Implements the "random" case for [setup_blood_traps]. See [make_blood_traps_random]
#' 
#' @inheritParams setup_blood_traps
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_traps.random = function(name, xds_obj, options = list(), s=1){
  blood_traps = make_blood_traps_random(xds_obj$nPatches, options)
  xds_obj <- change_blood_traps(blood_traps, xds_obj, s)
  return(xds_obj)
}

#' @title Make other blood hosts 
#' 
#' @description Set up a random vector 
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' 
#' @return a numeric vector
#' 
#' @export
make_blood_traps_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  blood_traps <- f_rand(nPatches, p1, p2)
  return(blood_traps)
})}

#' @title Set up a blood_traps
#' @description 
#' Pass a pre-configured blood_traps. If it passes 
#' the checks, it replaces the current blood_traps.
#' 
#' If called with `name = "as_is"`, the blood_traps
#' must be at `options$blood_traps` 
#' 
#' @inheritParams setup_blood_traps
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_blood_traps.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    blood_traps_as_is = options$blood_traps_asis
  if(is.numeric(name))
    blood_traps_as_is = as.vector(name)
  
  xds_obj <- change_blood_traps(blood_traps_as_is, xds_obj, s)
  
  return(xds_obj)
}

#' @title Update the blood_traps
#' @description Port function for the blood_traps, \eqn{\Theta}.
#' Dispatches on `class(xds_obj$XY_interface$traps_obj[[s]])`.
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_traps <- function(t, y, xds_obj, s){
  UseMethod("update_blood_traps",  xds_obj$patches$traps_obj[[s]])
}

#' @title Update the blood_traps
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_blood_traps
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_traps.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title blood_traps
#' 
#' @description The `S3` definition for the
#' function that computes blood_traps
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
blood_traps = function(t, y, xds_obj, s){
  UseMethod("blood_traps", xds_obj$XY_interface$traps_obj[[s]])
}

#' @title Compute availability of other blood hosts 
#'
#' @description This sets up... 
#' 
#' @inheritParams blood_traps
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
blood_traps.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Get availability of other blood hosts
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a numeric vector
#' @export
get_blood_traps = function(xds_obj, s=1){
  return(xds_obj$XY_interface$traps[[s]])
}


