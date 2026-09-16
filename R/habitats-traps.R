#' @title Check ovitraps
#' 
#' @description
#' Check that 
#' + all elements are non-negative
#' + length(ovitraps) == nPatches
#' 
#' 
#' @param ovitraps ovitrap availability
#' @param nP the number of patches
#' 
#' @seealso [xds_info_blood_feeding]
#' @export
check_ovitraps = function(ovitraps, nP){
  stopifnot(is.numeric(ovitraps))
  stopifnot(ovitraps >= 0)
  stopifnot(length(ovitraps) == nP) 
}

#' @title Change ovitrap availability

#' @description
#' Update the ovitraps for the \eqn{s^{th}} vector species 
#' 
#' @param ovitraps ovitrap availability
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' 
#' @export
change_ovitraps = function(ovitraps, xds_obj, s=1){
  check_ovitraps(ovitraps, xds_obj$nPatches)
  xds_obj$patches$ovitraps[[s]] <- ovitraps
  xds_obj$patches$ovitraps_obj[[s]] <- make_static_obj()
  xds_obj <- compute_Qall(xds_obj)
  xds_obj <- compute_O_matrix(xds_obj)
  return(xds_obj)
}

#' @title Set up ovitraps
#'
#' @param name a setup function name
#' @param xds_obj an **`xds`** model object
#' @param options configuration options
#' @param s the vector species index
#'
#' @return an **`xds`** object
#'
#' @export
setup_ovitraps = function(name, xds_obj, options = list(), s=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_ovitraps", options)
}

#' @title Set up ovitraps
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `OTname = name$name` 
#' + `options = name`
#' and call `setup_ovitraps(OTname, xds_obj, options, s)` 
#'
#' @inheritParams setup_ovitraps
#'
#' @return an **`xds`** object 
#' 
#' @keywords internal
#' @export
setup_ovitraps.list = function(name, xds_obj, options=list(), s=1){
  options = name
  OTname = name$name
  if(is.null(OTname)) OTname = "no_setup"
  xds_obj <- setup_ovitraps(OTname, xds_obj, options, s)
  return(xds_obj)
}

#' @title Set up no ovitraps 
#' @description Don't change anything 
#' @inheritParams setup_ovitraps
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_ovitraps.no_setup = function(name, xds_obj, options = list(), s=1){
  return(xds_obj)
}

#' @title Set up ovitraps
#' 
#' @description Set up random values for ovitrap availability
#' 
#' @inheritParams setup_ovitraps
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_ovitraps.random = function(name, xds_obj, options = list(), s=1){
  ovitraps = make_ovitraps_random(xds_obj$nPatches, options)
  xds_obj <- change_ovitraps(ovitraps, xds_obj, s)
  return(xds_obj)
}

#' @title Random ovitrap availability
#' 
#' @description Set up a random vector describing ovitrap availability
#' 
#' @param nPatches is the number of patches
#' @param options is a set of options that overwrites the defaults
#' @param f_rand a random number generator
#' @param p1 argument for f_rand 
#' @param p2 argument for f_rand
#' @return a numeric vector
#' @export
make_ovitraps_random = function(nPatches, options=list(), f_rand = stats::rlnorm, p1=0, p2=.5) {with(options,{
  ovitraps <- f_rand(nPatches, p1, p2)
  return(ovitraps)
})}

#' @title Set up ovitraps
#' 
#' @description 
#' Pass a pre-configured ovitraps. If it passes 
#' the checks, it replaces the current ovitraps.
#' 
#' If called with `name = "as_is"`, the ovitraps
#' must be at `options$ovitraps` 
#' 
#' @inheritParams setup_ovitraps
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_ovitraps.as_is = function(name, xds_obj, options=list(), s=1){
  
  if(is.list(options)) 
    ovitraps_as_is = options$ovitraps
  if(is.numeric(name))
    ovitraps_as_is = as.vector(name)
  
  xds_obj <- change_ovitraps(ovitraps_as_is, xds_obj, s)
  
  return(xds_obj)
}


#' @title Update ovitraps 
#' 
#' @description Port function for the ovitraps. 
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_ovitraps <- function(t, y, xds_obj, s){
  UseMethod("update_ovitraps", xds_obj$patches$ovitraps_obj[[s]])
}

#' @title Update ovitraps 
#' @description The `static` method returns `xds_obj` unmodified
#' @inheritParams update_ovitraps
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_ovitraps.static <- function(t, y, xds_obj, s) { return(xds_obj) }

#' @title Ovitraps
#' 
#' @description The `S3` definition for the
#' function that computes ovitraps
#' 
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
ovitraps = function(t, y, xds_obj, s){
  UseMethod("ovitraps", xds_obj$patches$ovitraps_obj[[s]])
}

#' @title Compute availability of ovitraps
#' 
#' @description Update nothing
#' 
#' @inheritParams ovitraps
#' 
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
ovitraps.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Get availability of ovitraps
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a [matrix]
#' @export
get_ovitraps = function(xds_obj, s=1){
  return(xds_obj$patches$ovitraps[[s]])
}


