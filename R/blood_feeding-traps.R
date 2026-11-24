#' @title Check blood trap availability
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(blood_traps) == nP
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
#' Set blood trap availability for vector species `s`.
#' 
#' @param blood_traps a numeric vector of patch-level availability
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#' 
#' @export
change_blood_traps = function(blood_traps, xds_obj, s=1){
  check_blood_traps(blood_traps, xds_obj$nPatches)
  xds_obj$patches$blood_traps[[s]] <- blood_traps
  xds_obj$patches$blood_traps_obj[[s]] <- make_static_obj()
  xds_obj$patches$blood_traps_obj[[s]]$blood_traps <- blood_traps
  return(xds_obj)
}

#' @title Set up blood traps
#'
#' @param name a setup method name or a numeric vector of availability values
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** model object
#'
#' @export
setup_blood_traps = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_blood_traps", options)
}

#' @title Set up blood traps
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `BTname = name$name`
#' + `options = name`
#' and call `setup_blood_traps(BTname, xds_obj, options, s)`
#'
#' @inheritParams setup_blood_traps
#'
#' @return an **`xds`** model object
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

#' @title Leave blood trap availability unchanged
#' @description Return `xds_obj` without changing the configured availability.
#' @inheritParams setup_blood_traps
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_blood_traps.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up blood traps
#' 
#' @description Generate random blood trap availability using
#' [make_blood_traps_random].
#' 
#' @inheritParams setup_blood_traps
#' @return an **`xds`** model object
#' @keywords internal
#' @export
setup_blood_traps.random = function(name, xds_obj, options = list(), s=1){
  blood_traps = make_blood_traps_random(xds_obj$nPatches, options)
  xds_obj <- change_blood_traps(blood_traps, xds_obj, s)
  return(xds_obj)
}

#' @title Make random blood trap availability
#' 
#' @description Generate a random vector of blood trap availability.
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overrides the defaults
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

#' @title Set up blood traps from supplied values
#' @description 
#' Pass a pre-configured blood_traps. If it passes 
#' the checks, it replaces the current blood_traps.
#' 
#' If called with `name = "as_is"`, the blood_traps
#' must be at `options$blood_traps_asis`.
#' 
#' @inheritParams setup_blood_traps
#' @return an **`xds`** model object
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

#' @title Update blood trap availability
#' @description Dispatch updates for blood trap availability.
#' Dispatches on `class(xds_obj$patches$blood_traps_obj[[s]])`.
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_blood_traps <- function(t, y, xds_obj, s){
  UseMethod("update_blood_traps",  xds_obj$patches$blood_traps_obj[[s]])
}

#' @title Update blood trap availability
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_blood_traps
#' @return an **`xds`** model object
#' @export
#' @keywords internal
update_blood_traps.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Blood trap update method
#' 
#' @description The `S3` definition for the
#' function that computes blood_traps
#' 
#' @param t time
#' @param y dependent variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @keywords internal
#' @export
blood_traps = function(t, y, xds_obj, s){
  UseMethod("blood_traps", xds_obj$patches$blood_traps_obj[[s]])
}

#' @title Update static blood trap availability
#'
#' @description The static method returns `xds_obj` unmodified.
#' 
#' @inheritParams blood_traps
#' 
#' @return an **`xds`** model object
#'
#' @keywords internal
#' @export
blood_traps.static = function(t, y, xds_obj, s){
  return(xds_obj)
}

#' @title Get blood trap availability
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a numeric vector
#' @export
get_blood_traps = function(xds_obj, s=1){
  return(xds_obj$patches$blood_traps[[s]])
}
